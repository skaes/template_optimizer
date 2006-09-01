# alpha version!
# hic sunt dracones!

require 'parse_tree'
require 'ruby2ruby'
require 'logger'
require 'pp'
require 'set'


# template compile time optimizer for ERB templates
class TemplateOptimizer

  # support for determining the defining module of method objects
  # (suggested by Nicholas Seckar)
  module GetModuleFromMethod
    # return defining module of self
    def module
      # puts "%%%%% in module: #{to_s}"
      return eval("::#{$1}") if /Method:\s+([A-Z]\w*)#/ =~ to_s
      return eval("::#{$1}") if /Method:\s+[A-Z]\w*\(([A-Z]\w*)\)#/ =~ to_s
      return eval("::#{$1}") if /Method:\s+#<Class:.*>\(([A-Z:a-z0-9]*)\)#/ =~ to_s
    end
  end

  [Method, UnboundMethod].each { |cls| cls.send(:include, GetModuleFromMethod) }


  # simple helper methods for abstract syntax trees
  module AST_Helpers

    # convert abstract syntax tree +ast+ to source
    def ast_source(ast)
      RubyToRuby.new.process(Sexp.from_array(ast))
    end

    # return a copy of a given abstract syntax tree. leafs are not copied.
    def deep_clone(ast)
      case ast
      when Array
        ast = ast.clone
        ast.collect!{|nd| deep_clone(nd) }
      when String
        ast = ast.clone
      end
      ast
    end

    # compute the size of a given abstract syntax tree
    def deep_size(ast)
      if ast.is_a?(Array)
        ast.inject(0){|size, nd| size += deep_size(nd) }
      else
        1
      end
    end

    # can a given abstract syntax tree be converted into a Ruby value?
    def is_a_constant?(ast)
      return false unless ast.is_a?(Array)
      case ast[0]
      when :str, :lit, :nil, :true, :false
        true
      when :dstr
        ast[2..-1].all? {|nd| is_a_constant?(nd) }
      when :array, :hash
        ast[1..-1].all? {|nd| is_a_constant?(nd) }
      else
        false
      end
    end

    #
    def is_true?(ast)
      is_a_constant?(ast) && build_constant(ast)
    end

    #
    def is_false?(ast)
      is_a_constant?(ast) && !(build_constant(ast))
    end

    # create a Ruby value from a given abstact syntax tree
    def build_constant(ast)
      case ast[0]
      when :nil        : nil
      when :true       : true
      when :false      : false
      when :str, :lit  : ast[1]
      when :dstr
        ast[1] + ast[2..-1].collect {|nd| build_constant(nd).to_s }.join("")
      when :array
        ast[1..-1].collect {|nd| build_constant(nd) }
      when :hash
        h = {}
        each_pair_in_hash_structure(ast) do |key, value|
          h[ build_constant(key) ] = build_constant(value)
        end
        h
      else
        raise "not a constant: #{ast.inspect}"
      end
    end

    # create an abstract syntax tree from a Ruby value
    def tree_from_constant(value)
      case value
      when NilClass  : [:nil]
      when TrueClass : [:true]
      when FalseClass: [:false]
      when Symbol    : [:lit, value]
      when String    : [:str, value]
      when Array     : [:array] + value.collect {|v| tree_from_constant(v) }
      when Hash
        value.inject([:hash]) do |ast, (k,v)|
          ast << tree_from_constant(k) << tree_from_constant(v)
        end
      else
        raise "unsupported constant: #{value.inspect}"
      end
    end

    # given abstract syntax tree +ast+, is it a hash structure where all keys
    # are constants?
    def constant_hash_domain?(ast)
      return false unless ast[0]==:hash
      1.step(ast.size-1, 2) do |i|
        return false unless is_a_constant?(ast[i])
      end
      true
    end

    # build a hash from a given +ast+ satisfying constant_hash_domain?(ast)
    # trees for the keys are converted to constants, the range values are
    # the corresponding abstract syntax trees
    def build_hash(ast)
      raise "not a hash tree #{ast.inspect}" unless ast[0]==:hash
      h = {}
      each_pair_in_hash_structure(ast) do |key, value|
        h[ build_constant(key) ] = value
      end
      h
    end

    # convert a hash into its abstract syntax tree representation
    # the range is left untouched
    def build_hash_tree(hash)
      # pp "§§§§§§§§§ #{hash.inspect}"
      hash.inject([:hash]){|ast,(k,v)| ast << tree_from_constant(k) << v }
    end

    # iterate over the (key, value) pairs of an ast representation for a hash structure
    def each_pair_in_hash_structure(ast)
      1.step(ast.size-1, 2) do |i|
        yield(ast[i], ast[i+1])
      end
    end

    # merge two abstract syntax trees with constant hash domains
    def merge_hashes_with_constant_domains(ast1, ast2)
      h1 = build_hash(ast1)
      h2 = build_hash(ast2)
      build_hash_tree(h1.merge(h2))
    end

    # given an ast representing a hash structure, delete a (key, value) pair
    # from the ast and return the value
    def delete_option(key, ast)
      return nil unless ast.is_a?(Array) && ast[0]==:hash
      result = nil
      1.step(ast.size-1, 2) do |i|
        if build_constant(ast[i]) == key
          result = ast[i+1]
          ast[i,2] = nil                 # delete key and value
          break
        end
      end
      result
    end

    # check wether a given ast representation of an interpolated string
    # can be coverted into a string value. returns either the original ast,
    # or the ast representation of the new string value
    #--
    # [:dstr, s0, e1, [:str, s1], e2, [:str, s2 ], ... ]
    # checks whether all e_i are of the form [:str, str_i]
    # and returns [:str, v], where v is the concatenation of all str_i
    def eval_const_dstr(ast)
      astclone = ast[1..-1]
      first_str = astclone.shift
      if astclone.all?{|x| x[0] == :str}
        [ :str, first_str + astclone.collect!{|a| a[1]}.join ]
      else
        ast
      end
    end

    # check whether a given +ast+ is method call on self with arguments.
    # return the arguments of the call as an array (whithout :array)
    #--
    # [:fcall, method, [:array, ...]]
    def fcall?(ast, method)
      if ast[0] == :fcall && ast[1] == method && ast[2][0] == :array
        ast[2][1..-1]
      end
    end

    # create an ast representing a method call on self. arguments optional.
    def fcall(method, *args)
      if args.length==0
        [:vcall, method]
      else
        [:fcall, method, [:array] + args]
      end
    end

    # check whether a given +ast+ is method call of the form expr.method
    # return the ast for expr
    #--
    # [:call, exp, method, ...]
    def method_call?(ast, method)
      if ast[0] == :call && ast[2] == method
        ast[1]
      end
    end

    # create an ast representing a method call on expr. arguments optional.
    def method_call(exp, method, *args)
      if args.length==0
        [:call, exp, method]
      else
        [:call, exp, method, [:array] + args]
      end
    end

    # given an ast representing a ruby block, merge simple subordinate block
    # into the embedding block
    def merge_blocks(ast)
      return ast unless ast.is_a?(Array) && ast[0] == :block
      i = 1
      while i<ast.length
        if (e = ast[i]) && (e[0] == :block)
          ast[i,1] = e[1..-1]
        else
          i += 1
        end
      end
      ast
    end

    # does +ast+ return a string?
    def returns_a_string(ast)
      return false unless ast.is_a?(Array)
      case ast[0]
      when :str, :dstr
        true
      when :fcall, :vcall
        CALLS_RETURNING_STRINGS.include?(ast[1])
      when :call
        ast[2]==:to_s || ast[2] == :to_param || ([:+, :<<, :concat].include?(ast[2]) && returns_a_string(ast[1]))
      end
    end

    # is +ast+ an inlinable call?
    def inlinable_call?(ast)
      return false unless ast.is_a?(Array)
      case ast[0]
      when :vcall, :fcall
        INLINE_CALLS.include?(ast[1])
      else
        false
      end
    end

    # extract parameter names from a block param section
    #--
    # [:iter,
    #  <method_call>,
    #  [:dasgn_curr, :text],   <------
    #  <block_body> ]
    #
    # or
    #
    # [:iter,
    #  <method_call>,
    #  [:masgn, [:array, [:dasgn_curr, :text1], [:dasgn_curr, :text2]]],   <------
    #  <block_body> ]
    def extract_block_params(block_params, params = [])
      return params unless block_params.is_a? Array
      case block_params[0]
      when :dasgn_curr
        params << block_params[1]
      else
        block_params.each{|nd| extract_block_params(nd, params) }
      end
      params
    end

  end # AST_Helpers


  # simple AST helper methods dealing with abstract syntax trees involving _erbout
  module ERB_Helpers

    # given abstract syntax tree +ast+, is it a _erbout.concat(expr) call?
    # returns tree for expr, if +ast+ matches above pattern
    def erb_concat?(ast)
      # [:call, [:lvar, :_erbout], :concat, [:array, tree]]
      if ast[0] == :call && ast[1][0] == :lvar && ast[1][1] == :_erbout && ast[2] == :concat
        return ast[3][1] if ast[3][0] == :array && ast[3].length==2
      end
    end

    # given abstract syntax tree +ast+, is it an assignment to +_erbout+?
    # returns tree for expr if +ast+ matches <tt>_erbout = expr</tt>.
    #--
    # [:lasgn, :_erbout, tree]
    def erb_assign?(ast)
      if ast[0] == :lasgn && ast[1] == :_erbout
        ast[2]
      end
    end

    # given abstract syntax tree +ast+, is it a <tt>_erbout.concat(expr)</tt> call,
    # where expr is a string constant?
    # returns tree for +expr+ if +ast+ matches above pattern.
    #--
    # [:call, [:lvar, :_erbout], :concat, [:array, [:str, "\n"]]]
    # returns "\n"
    def erb_str_concat?(ast)
      if ast[0] == :call && ast[1][0] == :lvar && ast[1][1] == :_erbout && ast[2] == :concat
        return ast[3][1][1] = ast[3][1][1].dup  if ast[3][0] == :array && ast[3][1][0] == :str
      end
    end

    # given abstract syntax tree +ast+, is it a _erbout.concat(expr) call,
    # where expr is a interpolated string?
    # returns either the first or last string constant inside the interpolated string,
    # depending on second parameter, which can be either :first or :last.
    #--
    # [:call, [:lvar, :_erbout], :concat, [:array, [:dstr, "...", ast, ...., [:str, "\n"]]]]
    # returns "\n" or "..."
    def erb_dstr_concat?(ast, which)
      if ast[0] == :call && ast[1][0] == :lvar && ast[1][1] == :_erbout && ast[2] == :concat
        if ast[3][0] == :array && ast[3][1][0] == :dstr
          case which
          when :first
            return ast[3][1][1] = ast[3][1][1].dup
          when :last
            last_elem = ast[3][1][-1]
            return last_elem[1] = last_elem[1].dup if last_elem[0] == :str
          end
        end
      end
    end

    # build an ast for <tt>_erbout.concat(exp)</tt>.
    def erb_concat(exp)
      method_call([:lvar, :_erbout], :concat, exp)
    end

    # check whether +ast+ is a local variable assigment.
    # return assigned variable and assigned exp.
    def lasgn?(ast)
      if ast[0] == :lasgn
        ast[1..-1]
      end
    end

    # check whether +ast+ is a local_assigns pattern
    # (as generated by the Rails template compiler).
    #--
    # [:if,
    #   [:call, [:lvar, :local_assigns], :has_key?, [:array, [:lit, :adminbar]]],
    #   [:lasgn, :adminbar,
    #      [:call, [:lvar, :local_assigns], :[], [:array, [:lit, :adminbar]]]],
    #   nil]
    def local_assign_pattern?(ast)
      if ast[0] == :if
        cond, stmt = ast[1], ast[2]
        l_assigns = [:lvar, :local_assigns]
        if method_call?(cond, :has_key?) == l_assigns
          key = cond[3]
          assigned_var, assigned_exp = lasgn?(stmt) # :aminbar, [:call, ...
          if assigned_var && assigned_exp && method_call?(assigned_exp, :[]) == l_assigns
            accessed_lit = assigned_exp[3]
            if key == accessed_lit &&  key[0] == :array && key[1][0] == :lit
              assigned_var if assigned_var == key[1][1]
            end
          end
        end
      end
    end

  end # ERB_Helpers


  # helpers for partial evaluation of methods involving hash arguments.
  module PartialEvaluation_Helpers

    def new_symbol_string
      "____#{@symbolic_value += 1}____"
    end

    def symbol_string?(s)
      s =~ /____\d+____/
    end

    # turn a string value with embedded symbol strings into an interpolated string
    # takes a mapping from symbols to abstract syntax trees.
    def substitute_symbols(str, symbols)
      return [:str, str] if symbols.size==0

      strings = str.split(/____.*?____/)
      vars    =  str.scan(/____.*?____/)

      ast = [:dstr, strings.shift]
      vars.length.times do
        sub_ast = deep_clone(symbols[vars.shift])
        ast << sub_ast << [:str, strings.shift || ""]
      end
      ast
    end

    # checks whether a given argument ast can be symbolized
    # fails, if ast contains a hash with non constant domain
    def symbolizable?(ast)
      return true if is_a_constant?(ast) || ast.nil?
      case ast[0]
      when :array
        ast[1..-1].all?{|nd| symbolizable?(nd)}
      when :hash
        each_pair_in_hash_structure(ast) do |k, v|
          is_a_constant?(k) && symbolizable?(v)
        end
      else
        true
      end
    end

    # given an ast representing a hash structure that will be processed by method
    # tag_options, check whether it can be symbolized.
    def symbolizable_html_options?(ast)
      return true if ast.nil?
      symbolizable = ast[0] == :hash
      each_pair_in_hash_structure(ast) do |k, v|
        symbolizable &&= symbolizable_html_option?(k,v)
      end
      symbolizable
    end

    # a html option k => v can be symbolized, if k is a constant and
    # boolean options are mapped to constants.
    def symbolizable_html_option?(k, v)
      if is_a_constant?(k)
        !%w(disabled readonly multiple).include?(build_constant(k).to_s) || is_a_constant?(v)
      end
    end

    # given an ast representing a hash structure that will be processed by method
    # url_for, check whether it can be symbolized.
    def symbolizable_url_options?(ast)
      return true if ast.nil?
      symbolizable = ast[0] == :hash
      each_pair_in_hash_structure(ast) do |k, v|
        symbolizable &&= symbolizable_url_option?(k,v)
      end
      symbolizable
    end

    # an url option k => v can be symbolized, if k is a constant and
    # "only_path" is mapped to a constant
    def symbolizable_url_option?(k, v)
      if is_a_constant?(k)
        build_constant(k).to_s != "only_path" || is_a_constant?(v)
      end
    end

    # given a symbolizable ast, a mapping from symbol strings to asts,
    # and proc to be applied to the range of the map, create a an ast
    # representing an interpolated string
    def build_structure(ast, symbols, proc)
      return nil if ast.nil?
      return build_constant(ast) if is_a_constant?(ast)
      case ast[0]
      when :array
        ast[1..-1].collect!{|nd| build_structure(nd, symbols)}
      when :hash
        h = {}
        each_pair_in_hash_structure(ast) do |k, v|
          h[build_constant(k)] = build_structure(v, symbols, proc)
        end
        h
      else
        key = new_symbol_string()
        symbols[key] = proc.call(ast)
        key
      end
    end

    # build structure for a url option hash or string
    def build_structure_for_url_options(ast, symbols)
      build_structure(ast, symbols, Proc.new {|nd| method_call(nd, :to_param) }) || {}
    end

    # build structure for a tag option hash
    def build_structure_for_html_options(ast, symbols)
      build_structure(ast, symbols, Proc.new {|nd| fcall(:h, nd) }) || {}
    end

    # build structure for an argument which is neither a url nor a html options hash
    def build_structure_for_arg(ast, symbols)
      build_structure(ast, symbols, Proc.new {|nd| nd })
    end

    # evaluate +method+ with +args+. replace symbol strings in result
    # using +symbols+ hash.
    def partially_evaluate_call(method, args, symbols)
      # pp "%%%%%%%% args #{args.inspect}\n"
      # pp "%%%%%%%% symbols #{symbols.inspect}\n"
      html_src = context.send(method, *args)
      # pp "%%%%%%%% html_src #{html_src}\n"
      new_ast = substitute_symbols(html_src, symbols)
      ## pp "%%%%%%%% new_ast #{new_ast.inspect}\n"
      new_ast
    end

    def nil_or_hash_or_str(type)#:nodoc:
      type.nil? || type == :hash || type == :str
    end

    def is_a_controller_string?(ast)
      # @log.puts "# testing for string: #{ast.inspect}" if @log
      ast and ast.is_a?(String) and !symbol_string?(ast)
    end

    # check whether +method+ involves a named route.
    def named_route_helper?(method)#:nodoc:
      # switch for old/new routing (r4393+)
      if defined? ActionController::CodeGeneration
        ActionController::Routing::NamedRoutes::Helpers.include?(method)
      else
        ActionController::Routing::Routes.named_routes.helpers.include?(method)
      end
    end

    # check whether +url_hash+ is safe for optimization. it is safe, if either
    # the optimized template will always be called with the same controller, or
    # the controller is specified in the +url_hash+.
    def safe_url_hash?(url_hash)
      !@needs_safe_url_hashes or
        is_a_controller_string?(url_hash["controller"]) or
        is_a_controller_string?(url_hash[:controller])
    end

  end # PartialEvaluation_Helpers

  # partial evaluators
  module PartialEvaluators

    # evaluator for +url_for+.
    #--
    # url_for(options = {}, *parameters_for_method_reference)
    def eval_url_for(ast, n, args, types, method)
      if n<2 && nil_or_hash_or_str(types[0]) && symbolizable_url_options?(args[0])
        as, symbols = [], {}
        as << build_structure_for_url_options(args[0], symbols)
        return ast unless safe_url_hash?(as[-1])
        ast = partially_evaluate_call(method, as, symbols)
      end
      ast
    end

    # evaluator for +link_to+.
    #--
    # link_to(name, options = {}, html_options = nil, *parameters_for_method_reference)
    def eval_link_to(ast, n, args, types, method)
      if n<4 && nil_or_hash_or_str(types[1]) &&
           symbolizable_url_options?(args[1]) && symbolizable_html_options?(args[2])
        as, symbols = [], {}
        as << build_structure_for_arg(args[0], symbols)
        as << build_structure_for_url_options(args[1], symbols)
        return ast unless safe_url_hash?(as[-1])
        as << build_structure_for_html_options(args[2], symbols)
        ast = partially_evaluate_call(method, as, symbols)
      end
      ast
    end

    # evaluator for +form_tag+ and +start_form_tag+.
    #--
    # form_tag(url_for_options = {}, options = {}, *parameters_for_url, &proc)
    def eval_form_tag(ast, n, args, types, method)
      if n==0 || (types[0] == :hash && (n==1 || (types[1] == :hash && (n==2 || types[2] == :hash))))
        if symbolizable_url_options?(args[0]) && symbolizable_html_options?(args[1])
          as, symbols = [], {}
          as << build_structure_for_url_options(args[0], symbols)
          return ast unless safe_url_hash?(as[-1])
          as << build_structure_for_html_options(args[1], symbols)
          ast = partially_evaluate_call(method, as, symbols)
        end
      end
      ast
    end

    # evaluator for +observe_form+, +observe_field+ and +draggable_element+.
    #--
    # observe_form(form_id, options = {})
    # observe_field(field_id, options = {})
    # draggable_element(element_id, options = {})
    # url parameters are passed via options[:url]
    def eval_observe_form(ast, n, args, types, method)
      if n==1 || types[1] == :hash
        new_args = deep_clone(args)
        url_options = delete_option(:url, new_args[1])
        if symbolizable_url_options?(url_options) && symbolizable?(new_args[1])
          as, symbols = [], {}
          as << build_structure_for_arg(new_args[0], symbols)
          options = build_structure_for_arg(new_args[1], symbols) || {}
          url_hash = build_structure_for_url_options(url_options, symbols)
          return ast unless safe_url_hash?(url_hash)
          options[:url] = url_hash if url_options
          as << options
          ast = partially_evaluate_call(method, as, symbols)
        end
      end
      ast
    end

    # evaluator for +form_remote_tag+.
    #--
    # form_remote_tag(options = {})
    # url parameters are passed via options[:url]
    # however, options[:html][:action] takes precedence over options[:url],
    # in which case url_for isn't called
    # this is one of the sickest interfaces I've ever seen
    def eval_form_remote_tag(ast, n, args, types, method)
      if n==0 || (n==1 && types[0] == :hash)
        new_args = deep_clone(args)
        html_entry = delete_option(:html, new_args[0])
        action_entry = delete_option(:action, html_entry) if html_entry
        url_entry = delete_option(:url, html_entry) if html_entry
        if symbolizable?(new_args[0]) && symbolizable_url_options?(url_entry) &&
                                         symbolizable_html_options?(html_entry)
          as, symbols = [], {}
          options = build_structure_for_arg(new_args[0], symbols)
          options[:html] = build_structure_for_arg(html_entry, symbols) if html_entry
          options[:html][:action]= build_structure_for_arg(action_entry, symbols) if action_entry
          url_hash = build_structure_for_url_options(url_entry, symbols)
          return ast unless action_entry || safe_url_hash?(url_hash)
          if url_hash
            options[:html] ||= {}
            options[:html][:url] = url_hash
          end
          as = [options]
          ast =  partially_evaluate_call(method, as, symbols)
        end
      end
      ast
    end

    # evaluator for +link_to_remote+.
    #--
    # link_to_remote(name, options = {}, html_options = {})
    # url parameters are passed via options[:url]
    def eval_link_to_remote(ast, n, args, types, method)
      if n==1 || (types[1] == :hash && (n==2 || (n==3 && types[2]==:hash)))
        new_args = deep_clone(args)
        url_options = delete_option(:url, new_args[1])
        if symbolizable_url_options?(url_options) && symbolizable?(new_args[0]) &&
                                                   symbolizable?(new_args[1]) && symbolizable_html_options?(new_args[2])
          as, symbols = [], {}
          as << build_structure_for_arg(new_args[0], symbols)
          options = build_structure_for_arg(new_args[1], symbols)
          url_hash = build_structure_for_url_options(url_options, symbols)
          return ast unless safe_url_hash?(url_hash)
          options[:url] = url_hash if url_options
          as << options
          as << build_structure_for_html_options(new_args[2], symbols)
          ast = partially_evaluate_call(method, as, symbols)
        end
      end
      ast
    end

    # evaluator for +cache+.
    #--
    # cache(name = {}, &block)
    def eval_cache(ast, n, args, types, method)
      if n==0 || (types[0] == :hash && symbolizable_url_options?(args[0]))
        symbols = {}
        options = build_structure_for_url_options(args[0], symbols)
        return ast unless safe_url_hash?(options)
        key = context.controller.fragment_cache_key(options)
        path = key.split("/")[1..-1].join("/")
        url_tree = substitute_symbols(path, symbols)
        cache_key_call = fcall(:simple_cache_key, url_tree)
        ast = fcall(:cache, cache_key_call)
      end
      ast
    end

    # evaluator for named route url.
    def eval_named_route(ast, n, args, types, method)
      new_args = deep_clone(args)
      if symbolizable_url_options?(new_args[0])
        as, symbols = [], {}
        as << {:only_path => true}.update(build_structure_for_url_options(new_args[0], symbols))
        if method.to_s =~ /^(.*)_url$/
          route_name = $1
          hash = context.send "hash_for_#{route_name}_url"
          return ast unless safe_url_hash? hash.merge(as[-1])
        end
        ast = partially_evaluate_call(method, as, symbols)
      end
      ast
    end

    # evaluator for named route hash.
    def eval_named_route_hash(ast, n, args, types, method)
      return ast unless n==0 || (n==1 && types[0]==:hash && constant_hash_domain?(args[0]))
      url_hash_tree = tree_from_constant(context.send(method))
      # puts "%%%%%%%%%%%% #{url_hash_tree.inspect}"
      ast = merge_hashes_with_constant_domains(url_hash_tree, deep_clone(args[0]) || [:hash])
      # puts  "%%%%%%%%%%%% #{ast.inspect}"
      ast
    end

    # select partial evaluator for +method+.
    def select_partial_evaluator(method)
      case method
      when :url_for, :link_to
        method
      when :form_tag, :start_form_tag
        :form_tag
      when :observe_form, :observe_field, :draggable_element
        :observe_form
      when :form_remote_tag
        method
      when :link_to_remote
        method
      when :cache
        method
      else
        if named_route_helper?(method)
          if method.to_s =~ /^hash_for_.*_url$/
            :named_route_hash
          else
            :named_route
          end
        end
      end
    end

  end # PartialEvaluators

  # helpers dealing with varable usage and substitution
  module Variable_Helpers

    # compute a hash, mapping variables to their use count
    def count_variable_usage(ast, kind, used_vars = Hash.new(0))
      return used_vars unless ast.is_a?(Array)
      if ast[0] == kind
        var_sym = ast[1]
        used_vars[var_sym] += 1
      else
        ast.each{|nd| count_variable_usage(nd, kind, used_vars) }
      end
      used_vars
    end

    # traverse ast and replace local variables by trees specified in map
    # be sure to copy the tree structure for each replament operation, to avoid
    # accidental sharing
    def substitute(ast, replacements)
      return ast unless ast.is_a?(Array)
      ast.collect!{|nd| substitute(nd, replacements)}
      case ast[0]
      when :lvar
        if new_ast = replacements[ast[1]]
          ast = deep_clone(new_ast)
        end
      end
      ast
    end

    # given a parameter declaration structure, and a list of trees as actuals arguments,
    # compute a hash, mapping variable names to trees.
    #--
    # [:x, :y, :z, [:block, [:lasgn, :y, [:lit, 5]], [:lasgn, :z, [:str, "huhu"]]]]
    # returns { :y => [:lit, 5], z => [:str, "huhu"] }
    def build_arguments_hash(formals, actuals)
      hash = {}
      defaults = formals[-1]
      if defaults.is_a?(Array)
        formals.delete_at(-1)
        defaults.shift
        defaults.each{|lasgn| hash[lasgn[1]] = lasgn[2] }
      end
      formals.each_with_index{|f,i| hash[f] = actuals[i] if i<actuals.length }
      hash
    end

    # return a fresh name variable name
    def get_local_name(symbol)
      "_#{symbol}_#{@local_counter += 1}".to_sym
    end

    # compute a variable renaming
    def get_local_variables(ast, var_map = {})
      return var_map unless ast.is_a?(Array)
      case ast[0]
      when :lasgn
        symbol = ast[1]
        if var_map[symbol]
          raise "already defined variable substitution: #{symbol}, map: #{var_map.inspect}"
        end
        ast[1] = var_map[symbol] = get_local_name(symbol)
      else
        ast.each{|nd| get_local_variables(nd, var_map) }
      end
      var_map
    end

    # rename local variables given a renaming
    def rename_local_variables(ast, var_map, args_hash)
      return ast unless ast.is_a?(Array)
      case ast[0]
      when :lvar
        symbol = ast[1]
        if new_symbol = var_map[symbol]
          ast[1] = new_symbol
        elsif !(args_hash.has_key? symbol)
          raise "undefined variable substitution: #{symbol}, map: #{var_map.inspect}"
        end
      end
      ast.collect!{|nd| rename_local_variables(nd, var_map, args_hash) }
    end

    # compute a variable renaming for block local variables
    # and change :dasgn_curr into :lasgn
    def get_block_local_variables(ast, var_map = {})
      return var_map unless ast.is_a?(Array)
      case ast[0]
      when :dasgn_curr
        symbol = ast[1]
        if var_map[symbol]
          raise "already defined variable substitution: #{symbol}, map: #{var_map.inspect}"
        end
        ast[0] = :lasgn
        ast[1] = var_map[symbol] = get_local_name(symbol)
      when :iter
        # nested block, only look at the method that gets passed the inner block
        get_block_local_variables(ast[1], var_map)
      else
        ast.each{|nd| get_block_local_variables(nd, var_map) }
      end
      var_map
    end

    # rename block local variables given a renaming
    # and change :dvar into :lvar
    def rename_block_local_variables(ast, var_map, args_hash)
      return ast unless ast.is_a?(Array)
      case ast[0]
      when :dvar
        symbol = ast[1]
        if new_symbol = var_map[symbol]
          # block local variable
          ast[0] = :lvar
          ast[1] = new_symbol
        elsif args_hash.has_key?(symbol)
          # parameter of inlined block
          ast[0] = :lvar
        else
          # parameter defined in outer block
          # do nothing
        end
      end
      ast.collect!{|nd| rename_block_local_variables(nd, var_map, args_hash) }
    end

  end # Variable_Helpers


  include AST_Helpers, ERB_Helpers, PartialEvaluation_Helpers, Variable_Helpers
  include PartialEvaluators


  cattr_accessor :logging
  @@logging = ARGV.include?('-Ol')

  cattr_accessor :log_dir
  @@log_dir = "#{RAILS_ROOT}/tmp/templates"

  # calls that should be inlined.
  # for now, only methods with a fixed number of arguments are supported.
  # default vaules for arguments work as well.
  INLINE_CALLS = Set.new []

  # constants that should be evaluated.
  EVALUATE_CONSTANTS = Set.new [ :RAILS_ENV, :RAILS_ROOT ]

  # calls that should be evaluated when all arguments are constant expressions.
  EVALUATE_CALLS = Set.new [
    :stylesheet_link_tag, :javascript_include_tag, :image_tag,
    :start_form_tag, :end_form_tag, :select_tag, :text_field_tag,
    :hidden_field_tag, :file_field_tag, :password_field_tag,
    :form_tag, :text_area_tag, :check_box_tag, :radio_button_tag,
    :submit_tag, :image_submit_tag, :link_to_function,
    :javascript_tag, :content_tag, :form_remote_tag, :link_to_remote,
    :observe_field, :h, :sortable_element, :toggle_effect ]

  # calls that will always return strings.
  CALLS_RETURNING_STRINGS = Set.new [
    :render, :render_to_string, :render_partial, :render_partial_collection,
    :render_partial_collection, :sub, :gsub, :pagination_links_each,
    :mail_to, :link_to, :url_for, :link_to_remote, :javascript_tag,
    :link_to_function, :h, :content_tag, :text_field, :check_box, :radio_button,
    :hidden_field, :file_field, :password_field,
    :distance_of_time_in_words_to_now, :text_area,
    :options_from_collection_for_select, :select, :render_component,
    :draggable_element, :js_distance_of_time_in_words_to_now ]

  # regexps for template methods which should not be optimized.
  IGNORED_METHODS = [
    /actionpack_lib_action_controller_templates_rescues/ ]

  # regexps for template methods which require "controller" in an url hash.
  METHODS_NEEDING_CONTROLLER_SPECS = [
    /_shared_/, /_layouts_application/ ]

  attr_reader :debug, :target, :method, :context

  def initialize(target, method, context, file_name=nil, debug=false)
    @target = target
    @method = method.to_sym
    @context = context
    @file_name = file_name.to_s || "#{@@log_dir}/#{method}.rb"
    @debug = debug
    @symbolic_value = 0
    @local_counter = 0
    @needs_safe_url_hashes = self.class.needs_safe_url_hashes?(@method.to_s)
  end

  # check whether template associated with +method_name+ should be ignored.
  # returns true if the method name matches any of the regexps in +IGNORED_METHODS+.
  def self.ignore_method?(method)
    method_name = method.to_s
    IGNORED_METHODS.any?{|regexp| method_name =~ regexp}
  end

  # check whether template associated with +method_name+ requires controller specs
  # for optimization. returns true if the method name matches any of the regexps in
  # +METHODS_NEEDING_CONTROLLER_SPECS+.
  def self.needs_safe_url_hashes?(method_name) #:nodoc:
    METHODS_NEEDING_CONTROLLER_SPECS.any?{|regexp| method_name =~ regexp}
  end

  # list of optimizations to perform. new methods are installed here.
  OPTIMIZATIONS = [
      :inline_calls, :optimize_renders, :remove_dead_code, :evaluate_constant_calls,
      :combine_strings, :optimize_form_for_and_fields_for, :optimize_form_fields,
      :partial_evaluation, :optimize_string_conversions, :remove_unused_local_assigns,
      :distribute_conditionals ]

  # maximium number of optimizer iterations to perform.
  ITERATIONS = 5

  # run optimizations given as parameter +optimizations+.
  # defaults to +OPTIMIZATIONS+.
  def optimize(optimizations = OPTIMIZATIONS)
    begin
      # turn off regexp recognition during optimization.
      ActionController::Routing.ignore_regexps = true

      @log = File.open("#{@@log_dir}/#{method}.log", "w") if @debug

      tree = ParseTree.new.parse_tree_for_method(target, method)
      if @debug
        @log.puts '# -*- ruby -*-'
        @log.puts "#================================================="
        @log.puts "# original tree:"
        @log.puts "#-------------------------------------------------"
        PP.pp tree, @log
      end

      # inline method calls needs to be called only once.
      # other optimizer passes do not produce inlinable calls.
      tree = optimizer_pass(tree, :inline_calls, @log)

      looped_optimizers = optimizations -
            [:inline_calls, :optimize_erbout, :remove_unused_local_assigns]

      1.upto(ITERATIONS) do |i|
        old_tree = deep_clone(tree)
        looped_optimizers.each do |optimizer|
          tree = optimizer_pass(tree, optimizer, @log)
        end
        if old_tree == tree
          @log.puts "# no optimizer change after #{i} iterations" if @log
          break
        end
        if i == ITERATIONS && @log
          @log.puts "# still optimizer changes after #{i} iterations"
          @log.puts "# increase TemplateOptimizer::ITERATIONS to get better results"
        end
      end

      # optimize_erbout and remove_unused_locals need to be called only once
      # as they don't enable other optimizations.
      tree = optimizer_pass(tree, :optimize_erbout, @log)
      tree = optimizer_pass(tree, :remove_unused_local_assigns, @log)

      new_source = ast_source(tree)

      File.open(@file_name, "w") do |f|
        # f.puts '# -*- ruby -*-'
        f.puts new_source
      end

      target.module_eval(new_source, @file_name, 0)

      new_source
    ensure
      @log.close unless @log.nil?
      ActionController::Routing.ignore_regexps = false
    end
  end

  # run an optimizer +method+ on given +tree+. log to +file+ unless <tt>file.nil?</tt>.
  def optimizer_pass(tree, method, file=nil)
    unless @debug && self.class.logging && !file.nil?
      self.send method, tree
    else
      file.puts "#================================================="
      file.puts "# new tree for optimizer_pass: #{method}"
      file.puts "# -------------------------------------------------"
      tree = self.send method, tree
      PP.pp tree, file
      file.puts "# ================================================="
      file.puts "# new source for optimizer_pass: #{method}"
      file.puts "# -------------------------------------------------"
      file.puts ast_source(tree)
      tree
    end
  end

  # hash method calls which can be partially executed if both hashes
  # have constant domains.
  MERGABLE_HASH_METHODS = Set.new [:merge, :update, :merge!]

  # traverse +ast+ and evaluate constants specified in +EVALUATE_CONSTANTS+ and
  # calls specified in +EVALUATE_CALLS+. Additionally hashes with constant domains
  # are merged.
  def evaluate_constant_calls(ast)
    return ast unless ast.is_a?(Array)
    ast.collect! {|nd| evaluate_constant_calls(nd) }
    case ast[0]
    when :vcall
      # [:vcall, v]
      if EVALUATE_CALLS.include?(ast[1])
        ast = tree_from_constant(context.send(ast[1]))
      end
    when :fcall
      # [:fcall, method, [:array, e1, ..., en]]
      if EVALUATE_CALLS.include?(ast[1]) && is_a_constant?(ast[2])
        args = build_constant(ast[2])
        ast = tree_from_constant(context.send(ast[1], *args))
      end
    when :call
      # [:call, e0, method, [:array, e1, .. en]
      if ast[2] == :to_param && is_a_constant?(ast[1]) && ast[1][0]==:str
        ast = tree_from_constant(build_constant(ast[0]).to_param)
      elsif MERGABLE_HASH_METHODS.include?(ast[2]) &&
            constant_hash_domain?(ast[1]) && constant_hash_domain?(ast[3][1])
        ast = merge_hashes_with_constant_domains(ast[1], ast[3][1])
      elsif constant_hash_domain?(ast[1]) && ast[2] == :[] && is_a_constant?(ast[3][1])
        ast = build_hash(ast[1])[build_constant(ast[3][1])]
      end
    when :const
      # [:const, :RAILS_ENV]
      if EVALUATE_CONSTANTS.include?(ast[1])
        ast = tree_from_constant(context.instance_eval(ast[1].to_s))
      end
    end
    ast
  end

  # create an ast representing an inlined call of +method+ with +args+.
  def inline_call(method, args)
    # @log.puts "%%%% trying to inline: #{method}" if @log
    # @log.puts "context respond_to(#{method})?: #{context.respond_to?(method)}" if @log
    defining_module = context.class.instance_method(method).module
    # @log.puts "defining_module #{defining_module}" if @log
    tree = ParseTree.new.parse_tree_for_method(defining_module, method)
    # PP.pp tree, @log if @log
    # [:defn, :method, [:scope, [:block, [:args, ...], ...]]]
    block = tree[2][1]
    formal_args = block[1]
    args_hash = build_arguments_hash(formal_args[1..-1], args)
    block.delete_at(1)
    # @log.puts "%%%% args_hash: #{args_hash.inspect}" if @log
    locals = get_local_variables(block)
    # @log.puts "%%%% locals: #{locals.inspect}" if @log
    rename_local_variables(block, locals, args_hash)
    ast = substitute(block, args_hash)
    if ast.length == 2
      ast[1]
    else
      ast
    end
  end

  # evaluate yields with a given +block_body+, where +params+ are the block's parameters
  def inline_yields(ast, params, block_body)
    return ast unless ast.is_a? Array
    ast.collect!{|nd| inline_yields nd, params, block_body }
    case ast[0]
    when :yield
      block_body = deep_clone(block_body)
      # @log.puts "%%%%% trying to inline yield: params=#{params.inspect}, yield=" if @log
      # PP.pp ast, @log if @log
      # @log.puts "%%%%% block_body:" if @log
      # PP.pp block_body, @log if @log
      actuals = ast[1][0]==:array ? ast[1][1..-1] : [ast[1]]
      arg_pairs = params.zip(actuals)
      # @log.puts "%%%%% arg_pairs: #{arg_pairs.inspect}" if @log
      args_hash = arg_pairs.inject({}){|hash, (formal,actual) | hash[formal] = actual; hash}
      # @log.puts "%%%%% substitution= #{args_hash.inspect}" if @log
      block_locals = get_block_local_variables(block_body)
      # @log.puts "%%%% block_locals: #{block_locals.inspect}" if @log
      rename_block_local_variables(block_body, block_locals, args_hash)
      # @log.puts "%%%%% block with dvars renamed:" if @log
      # PP.pp block_body, @log if @log
      ast = substitute(block_body, args_hash)
      # @log.puts "%%%%% result:" if @log
      # PP.pp ast, @log if @log
    end
    ast
  end

  # inline a block into an inlined method call with embedded yields
  #--
  # [:iter, method_call, block_params, block_body]
  def inline_block(method_call, block_params, block_body)
    params = extract_block_params(block_params)
    if block_body[0]==:block
      block_body.delete_at(1)
    end
    inline_yields(method_call, params, block_body)
  end

  # traverse +ast+ and inline calls specified in +INLINE_CALLS+.
  def inline_calls(ast)
    return ast unless ast.is_a?(Array)
    case ast[0]
    when :vcall
      if INLINE_CALLS.include?(ast[1])
        ast = inline_call(ast[1], [])
        ast = inline_calls(ast)
      else
        ast.collect! {|nd| inline_calls(nd) }
      end
    when :fcall
      if INLINE_CALLS.include?(ast[1])
        ast = inline_call(ast[1], ast[2] ? ast[2][1..-1] : [])
        ast = inline_calls(ast)
      else
        ast.collect! {|nd| inline_calls(nd) }
      end
    when :iter
      if inlinable_call?(ast[1])
        ast[1] = inline_calls(ast[1])
        ast = inline_block(ast[1], ast[2], ast[3])
        ast = inline_calls(ast)
      else
        ast.collect! {|nd| inline_calls(nd) }
      end
    else
      ast.collect! {|nd| inline_calls(nd) }
    end
    ast
  end

  # traverse +ast+ and remove unreachable code.
  def remove_dead_code(ast)
    return ast unless ast.is_a?(Array)
    ast = ast.inject([]) do |new_ast, nd|
      new_child = remove_dead_code(nd)
      new_ast << new_child if nd.nil? || !new_child.nil?  # WTF?
      new_ast
    end
    case ast[0]
    when :const
      begin
        condition = eval "#{ast[1]}"
        # puts "condition=#{ast[1]},#{condition}"
        case condition
        when NilClass  : ast = [:nil]
        when FalseClass: ast = [:false]
        when TrueClass : ast = [:true]
        end
      rescue NameError
      end
    when :if
      case condition = ast[1][0]
      when :true        then ast = ast[2]
      when :false, :nil then ast = ast[3] if ast.length==4 # ignore elsif
      end
    when :and
      if is_false?(ast[1])
        ast = [:nil]
      elsif is_true?(ast[1])
        ast = ast[2]
      end
    when :or
      if is_true?(ast[1])
        ast = ast[1]
      elsif is_false?(ast[1])
        ast = ast[2]
      end
    when :block
      ast = merge_blocks ast
    end
    ast
  end

  # traverse +ast+ and replace calls of the form <tt>exp.to_s</tt> by +exp+,
  # if +exp+ is known to return a string.
  def optimize_string_conversions(ast)
    return ast unless ast.is_a?(Array)
    ast.collect! {|nd| optimize_string_conversions(nd) }
    case ast[0]
    when :call
      if ast[2] == :to_s
        ast = ast[1] if returns_a_string(ast[1])
      end
    end
    ast
  end

  # traverse +ast+ and combine erb string concatenations.
  #    _erbout.concat(a); _erbout.concat(b) ==> _erbout.concat(a + b)
  def combine_strings(ast)
    return ast unless ast.is_a?(Array)
    ast.collect! {|nd| combine_strings(nd) }
    begin
    case ast[0]
    when :block
      i = 1
      while i<(n=ast.length)
        if (str1 = erb_str_concat?(ast[i])) && (i+1)<n
          if str2 = erb_str_concat?(ast[i+1])
            str1 << str2
            ast.delete_at(i+1)
          elsif str2 = erb_dstr_concat?(ast[i+1], :first)
            str2.insert(0, str1)
            ast.delete_at(i)
          else
            i += 1
          end
        elsif (str1 = erb_dstr_concat?(ast[i], :last)) && (i+1)<n
          if str2 = erb_str_concat?(ast[i+1])
            str1 << str2
            ast.delete_at(i+1)
          else
            i += 1
          end
        else
          i += 1
        end
      end
    when :dstr
      ast = eval_const_dstr(ast)
    when :call
      if ast[2] == :+ && ast[1][0] == :str && ast[3][0] == :array && ast[3][1][0] == :str
        ast = [:str, ast[1][1] + ast[3][1][1]]
      end
    end
    rescue Exception => e
      puts "Exception"
      puts e
      puts ast.inspect
    end
    ast
  end

  # optimize a single render call.
  #--
  # [:fcall, :render, [:array, [:hash, ...]]]
  def optimize_render(ast) #:nodoc:
    args_hash = build_hash(ast[2][1])
    partial = args_hash.delete(:partial)
    object = args_hash.delete(:object)
    collection = args_hash.delete(:collection)
    locals = args_hash.delete(:locals)
    spacer_template = args_hash.delete(:spacer_template)
    return ast unless args_hash.empty? && partial
    new_args = [partial]
    if collection
      render_sym = :render_partial_collection
      new_args << collection
      if locals
        new_args << (spacer_template || [:nil])
        new_args << locals
      elsif spacer_template
        new_args << spacer_template
      end
    else
      render_sym = :render_partial
      if locals
        new_args << (object || [:nil])
        new_args << locals
      else
        new_args << object if object
      end
    end
    fcall(render_sym, *new_args)
  end

  # traverse +ast+ and simplify render calls. the params hash of render
  # must be a subset of the following keys: <tt>:object</tt>, <tt>:partial</tt>,
  # <tt>:locals</tt>, <tt>:collection</tt> and <tt>:spacer_template</tt>.
  #
  #    render(:partial => exp1, :object => exp2)
  #      ==> render_partial(exp1, exp2)
  #
  #    render(:partial => exp1, :collection => exp2)
  #      ==> render_partial_collection(exp1, exp2)
  #
  def optimize_renders(ast)
    return ast unless ast.is_a?(Array)
    ast.collect! {|nd| optimize_renders(nd) }
    case ast[0]
    when :fcall
      if ast[1] == :render && ast[2][1][0] == :hash
        ast = optimize_render(ast)
      end
    end
    ast
  end

  # move erb concats inside if branches, i.e., change
  #   _erbout.concat( (if c then e_1 else e_2 end).to_s )
  # into
  #    if c then _erbout.concat(e_1.to_s) else _erbout.concat(e_2.to_s) end
  def distribute_conditionals(ast)
    return ast unless ast.is_a?(Array)
    ast.collect! {|nd| distribute_conditionals(nd) }
    if (conc = erb_concat?(ast)) && (exp = method_call?(conc, :to_s)) && (exp[0] == :if)
      if exp[2].nil?
        ast = [:if, exp[1], nil, erb_concat(method_call(exp[3], :to_s))]
      elsif exp[3].nil?
        ast = [:if, exp[1], erb_concat(method_call(exp[2], :to_s)), nil]
      elsif exp.length==4
        ast = [:if, exp[1],
               erb_concat(method_call(exp[2], :to_s)),
                  erb_concat(method_call(exp[3], :to_s)) ]
      end
    end
    ast
  end

  # if possible, remove the initial assignment to _erbout and the final return of _erbout
  #--
  #  [:defn, :_run_rhtml_welcome_index,
  #   [:scope,
  #    [:block,
  #     [:args, :local_assigns],
  #     ...
  #     [:lasgn, :_erbout, [:str, ""]],
  #     [:call, [:lvar, :_erbout], :concat, [:array, ....] ]
  #     ...
  #     [:call, [:lvar, :_erbout], :concat, [:array, ....] ]
  #     [:lvar, :_erbout]]]]
  def optimize_erbout(ast)
    block = ast[2][1]
    if block[-1] == [:lvar, :_erbout] && erb_concat?(block[-2])
      block.pop
    end
    i = block.index [:lasgn, :_erbout, [:str, ""]]
    expr_after_init = block[i+1]
    if expr_after_init && (concatenated_expr = erb_concat?(expr_after_init))
      block[i][2] = concatenated_expr
      block.delete_at(i+1)
    end
    if assigned_expr = erb_assign?(block[-1])
      block[-1] = assigned_expr
    end
    ast
  end

  # remove unused local assigns.
  # in particular, the partial counters are almost never used.
  #--
  #[:defn, :_run_rhtml_shared__adminbar,
  #   [:scope,
  #    [:block,
  #     [:args, :local_assigns],
  #     [:if,
  #      [:call, [:lvar, :local_assigns], :has_key?, [:array, [:lit, :adminbar]]],
  #      [:lasgn,
  #       :adminbar,
  #       [:call, [:lvar, :local_assigns], :[], [:array, [:lit, :adminbar]]]],
  #      nil],
  #     [:if,
  #      [:call,
  #       [:lvar, :local_assigns],
  #       :has_key?,
  #       [:array, [:lit, :adminbar_counter]]],
  #      [:lasgn,
  #       :adminbar_counter,
  #       [:call,
  #        [:lvar, :local_assigns],
  #        :[],
  #        [:array, [:lit, :adminbar_counter]]]],
  #      nil],
  #      ...
  def remove_unused_local_assigns(ast)
    block = ast[2][1]
    if block[1] == [:args, :local_assigns]
      variable_usage_count = count_variable_usage(block[2..-1], :lvar)
      i = 2
      while local_var = local_assign_pattern?(block[i])
        if variable_usage_count[local_var]==0
          block.delete_at(i)
        else
          i +=1
        end
      end
    end
    ast
  end

  # partial evalution of calls involving urls and/or html options.
  def partial_evaluation(ast)
    return ast unless ast.is_a?(Array)
    ast.collect! {|nd| partial_evaluation(nd) }
    case ast[0]
    when :fcall, :vcall
      # [:fcall, method, args] or [:vcall, method]
      if ast.size==2 # no arguments given
        args = []
      else
        next unless ast[2][0] == :array
        args = ast[2][1..-1]
      end
      n = args.length
      types = args.map{|nd| nd[0]}
      method = ast[1]
      if evaluator = select_partial_evaluator(method)
        ast = send "eval_#{evaluator}", ast, n, args, types, method
      end
    end
    ast
  end

  # class MockField is used by the form field optimizer.
  class MockField

    def create_method(name, &block)
      self.class.send(:define_method, name, &block)
    end

    def initialize(method_name, field_name, field_type, options={})
      create_method(method_name) { value }
      @field_name = field_name
      @field_type = field_type
      @method_name = method_name
      @options = options
    end

    def id_before_type_cast
      "___mock___id_for_template_optimizer___mock___"
    end

    def value
      "___mock___value_for_template_optimizer___mock___"
    end

    def create_ast(field_string)
      field_string.sub!(/(checked="checked")/, '___mock___\1___mock___')
      a = field_string.split(/___mock___/)
      a.insert(0, "") if a[0] =~ /_for_template_optimizer/
      # a.push("") if a[-1] =~ /_for_template_optimizer/
      ast = [:dstr, a.shift]
      a.each do |s|
        case s
        when 'id_for_template_optimizer'
          ast << method_call(var_tree, :id_before_type_cast)
        when 'value_for_template_optimizer'
          ast << fcall(:h, get_attr_call)
        when 'checked="checked"'
          if @options[:explicit_check]
            if @options[:checked] == true || @options[:checked] == "checked"
              ast << [:str, s]
            end
          else
            check_method = @field_type==:radio_button ? :radio_button_check : :check_box_check
            ast << check_attr_call(check_method)
          end
        else
          ast << [:str, s]
        end
      end
      ast
    end

    private

    include AST_Helpers

    def var_tree #:nodoc:
      @options[:object_ast] || [:ivar, "@#{@field_name}".to_sym]
    end

    def method_lit #:nodoc:
      [:lit, @method_name.to_sym]
    end

    def get_attr_call #:nodoc:
      method_call([:const, HELPER_CLASS], :safe_object_value, var_tree, method_lit)
    end

    def check_attr_call(method) #:nodoc:
      method_call([:const, HELPER_CLASS], method, var_tree, method_lit, @options[:checked_ast])
    end

    HELPER_CLASS = "ActionView::Helpers::InstanceTag".to_sym #:nodoc:
  end

  # optimizable form field helpers.
  OPTIMIZABLE_FORM_HELPER_METHODS = Set.new [
    :text_area, :text_field, :hidden_field, :file_field, :password_field,
    :check_box, :radio_button ]


  class FieldOptimizer

    # collect everthing needed to optimize +ast+.
    def initialize(ast)
      # [:fcall, method, [:array, e1, ..., en]]
      @optimizable = false
      return unless ast[2] && ast[2][0] == :array
      @args = ast[2][1..-1]
      @method = ast[1]
      # known form helper method?
      return unless OPTIMIZABLE_FORM_HELPER_METHODS.include?(method)

      @name_ast = args[0]
      @method_ast = args[1]

      # name and method are constants?
      # puts "%%%% name: #{name_ast.inspect} constant?: #{is_a_constant?(name_ast)}"
      # puts "%%%% name: #{method_ast.inspect} constant?: #{is_a_constant?(method_ast)}"
      return unless is_a_constant?(name_ast) && is_a_constant?(method_ast)

      if method == :radio_button
        @options_ast = args[3] || [:hash]
      else
        @options_ast = args[2] || [:hash]
      end

      # options hash has a constant domain?
      # puts "%%%% options: #{options_ast.inspect} constant domain?: #{constant_hash_domain?(options_ast)}"
      return unless constant_hash_domain?(options_ast)
      @options_hash = build_hash(options_ast)
      @object_ast = options_hash.delete(:object)
      @options_tree = build_hash_tree(options_hash)

      # options hash without :object is a constant?
      # puts "%%%% options hash: #{options_tree.inspect} constant?: #{is_a_constant?(options_tree)}"
      return unless is_a_constant?(options_tree)

      # ready to go!
      @tf_name = build_constant(name_ast)
      @tf_method = build_constant(method_ast)
      @tf_options = build_constant(options_tree)

      @optimizable = true
      @ast = ast
    end

    def optimizable?
      @optimizable
    end

    # run optimization for collected field info.
    def optimize(context)
      case method
      when :text_area, :text_field, :hidden_field, :file_field, :password_field
        optimize_text_field(context)
      when :check_box
        optimize_check_box(context)
      when :radio_button
        optimize_radio_button(context)
      else
        raise "don't know how to optimize this field: #{self.inspect}"
      end
    end

    private

    include AST_Helpers

    attr_reader :ast, :args, :method, :name_ast, :method_ast, :options_ast
    attr_reader :options_hash, :object_ast, :options_tree
    attr_reader :tf_name, :tf_method, :tf_options

    # optimize simple field
    #--
    # text_field(name, method, options = {})
    # [:fcall, :text_field, [:array, name_ast, method_ast, options_ast]]
    def optimize_text_field(context)
      mock_field = MockField.new(tf_method, tf_name, method,
                                 :object_ast => object_ast)
      mocked_options = tf_options.merge(:object => mock_field)
      field_string = context.send(method, tf_name, tf_method, mocked_options)
      mock_field.create_ast(field_string)
    end

    # optimize check box field
    #--
    # check_box(object_name, method, options = {}, checked_value = "1", unchecked_value = "0")
    # [:fcall, :check_box, [:array, name_ast, method_ast, options_ast, checked_ast, unchecked_ast]]
    def optimize_check_box(context)
      checked_ast = args[3] || [:str, "1"]
      unchecked_ast = args[4] || [:str, "0"]
      return ast unless is_a_constant?(checked_ast) && is_a_constant?(checked_ast)

      tf_checked_value = build_constant(checked_ast)
      tf_unchecked_value = build_constant(unchecked_ast)

      tf_checked = tf_options["checked"]
      explicit = tf_options.has_key?("checked")
      tf_options["checked"] = "checked"

      mock_field = MockField.new(tf_method, tf_name, method,
                                 :object_ast => object_ast,
                                 :explicit_check => explicit,
                                 :checked => tf_checked,
                                 :checked_ast => checked_ast)
      mocked_options = tf_options.merge(:object => mock_field)
      field_string = context.send(method, tf_name, tf_method, mocked_options,
                                  tf_checked_value, tf_unchecked_value)
      mock_field.create_ast(field_string)
    end

    # optimize radio button field
    #--
    # radio_button(object_name, method, tag_value, options = {})
    # [:fcall, :radio_button, [:array, name_ast, method_ast, value_ast, options_ast]]
    def optimize_radio_button(context)
      value_ast = args[2]
      return ast unless is_a_constant?(value_ast)
      tf_value = build_constant(value_ast)

      tf_checked = tf_options["checked"]
      explicit = tf_options.has_key?("checked")
      tf_options["checked"] = "checked"

      mock_field = MockField.new(tf_method, tf_name, method,
                                 :object_ast => object_ast,
                                 :explicit_check => explicit,
                                 :checked => tf_checked,
                                 :checked_ast => value_ast)
      mocked_options = tf_options.merge(:object => mock_field)
      field_string = context.send(method, tf_name, tf_method, tf_value, mocked_options)
      mock_field.create_ast(field_string)
    end

  end

  # traverse +ast+ and optimize calls to form field helpers.
  def optimize_form_fields(ast)
    return ast unless ast.is_a?(Array)
    ast.collect! {|nd| optimize_form_fields(nd) }
    case ast[0]
    when :fcall
      foo = FieldOptimizer.new(ast)
      ast = foo.optimize(context) if foo.optimizable?
    end
    ast
  end

  # inline new form helpers +form_for+ and +fields_for+.
  # calls to +form_for+ will always be replaced.
  # replacement of <tt>fields_for ... |f|</tt> requires that all
  # occurences of +f+ can be eliminated.
  #--
  #[:iter,
  # [:fcall,
  #  :form_for,
  #  [:array,
  #   [:lit, :user],
  #   [:ivar, :@user],
  #   [:hash, [:lit, :url], [:str, "create"]]]],
  # [:dasgn_curr, :f],
  # [:block,
  #  [:call,
  #   [:lvar, :_erbout],
  #   :concat,
  #   [:array,
  #    [:call,
  #     [:call,
  #      [:dvar, :f],
  #      :text_field,
  #      [:array, [:lit, :name], [:hash, [:str, "size"], [:lit, 20]]]],
  #     :to_s]]],
  def optimize_form_for_and_fields_for(ast)
    return ast unless ast.is_a?(Array)
    case ast[0]
    when :iter
      if (args = fcall?(ast[1], :form_for)) && constant_hash_domain?(args[2])
        options = build_hash(args[2])
        return ast unless Set.new(options.keys).subset?(Set.new([:url,:html,:builder]))
        url_options = options.delete :url
        html_options = options.delete :html
        options_tree = build_hash_tree(options)
        start_form_args = [url_options || [:hash], html_options || [:hash]]
        start_form = erb_concat fcall(:start_form_tag, *start_form_args)
        end_form = erb_concat [:str, "</form>"]
        fields_for = fcall(:fields_for, args[0], args[1], options_tree)
        new_iter = [:iter, fields_for, ast[2], ast[3]]
        ast = [:block, start_form, new_iter, end_form]
      elsif (args = fcall?(ast[1], :fields_for)) && constant_hash_domain?(args[2])
        options = build_hash(args[2])
        return ast unless options.keys.empty?
        var_tree = [:dvar, ast[2][1]]
        object_name = args[0]
        object_tree = args[1]
        body = replace_new_fields(deep_clone(ast[3]), var_tree, object_name, object_tree)
        ast = body if count_variable_usage(body, :dvar).empty?
      end
    end
    ast.collect! {|nd| optimize_form_for_and_fields_for(nd) }
    ast
  end

  # optimizable form field helpers used by +fields_for+.
  NEW_FORM_HELPER_METHODS = Set.new.merge OPTIMIZABLE_FORM_HELPER_METHODS

  # replace calls to new form helper methods by old ones, using the :object option
  #   f.text_field(:name, "size" => 20)
  # will be replaced by
  #   text_field(:user, :name, "size" => 20, :object => @user)
  # if ocurring inside a <tt>form_for :user, @user do |f| ...</tt> block
  def replace_new_fields(ast, var_tree, object_name, object_tree)
    return ast unless ast.is_a?(Array)
    ast.collect! {|nd| replace_new_fields(nd, var_tree, object_name, object_tree) }
    case ast[0]
    when :call
      if ast[1]==var_tree && NEW_FORM_HELPER_METHODS.include?(method = ast[2]) && ast[3][0] == :array
        # [:call, [:dvar, :f], :text_field,
        #     [:array, [:lit, :email], [:hash, [:str, "size"], [:lit, 50]]]]
        args = ast[3][1..-1]
        field_name, options = args[0], (args[1] || [:hash])
        options_hash = { :object => deep_clone(object_tree) }.merge(build_hash(options))
        options_tree = build_hash_tree(options_hash)
        ast = fcall(method, object_name, field_name, options_tree)
      end
    end
    ast
  end

end


__END__

#  Copyright (C) 2006  Stefan Kaes
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
