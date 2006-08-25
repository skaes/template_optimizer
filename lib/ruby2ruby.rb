# *** IMPORTANT NOTE ***
# the code in this file was derived from Ryn Davis' ruby2ruby script.
# It is distributed under Ryan Davis' original licencse,
# unlike the remaiing code of the template optimizer, which is GPL licensed.

require 'parse_tree'
require 'sexp_processor'

class RubyToRuby < SexpProcessor

  def self.translate(klass, method=nil)
    unless method.nil? then
      self.new.process(ParseTree.new(false).parse_tree_for_method(klass, method))
    else
      self.new.process(ParseTree.new(false).parse_tree(klass).first) # huh? why is the :class node wrapped?
    end
  end

  def initialize
    super
    @indent = "  "
    self.auto_shift_type = true
    self.strict = true
    self.expected = String
  end

  def indent(s)
    s.to_s.map{|line| @indent + line}.join
  end

  def process_and(exp)
    "(#{process exp.shift} and #{process exp.shift})"
  end

  def process_or(exp)
    "(#{process exp.shift} or #{process exp.shift})"
  end

  def process_not(exp)
    "(not #{process exp.shift})"
  end

  def process_defined(exp)
    "defined?(#{process exp.shift})"
  end

  def process_args(exp)
    args = []

    until exp.empty? do
      arg = exp.shift
      if arg.is_a? Array
        args[-(arg.size-1)..-1] = arg[1..-1].map{|a| process a}
      else
        args << arg
      end
    end

    return "(#{args.join ', '})"
  end

  def process_array(exp)
    "[" + arg_list(exp) + "]"
  end

  def process_attrasgn(exp)
    process_call(exp)
  end

  def process_block(exp)
    code = []
    until exp.empty? do
      code << process(exp.shift)
    end

    body = code.join("\n")
    body += "\n"

    return body
  end

  def process_block_pass(exp)
    block = process(exp.shift)
    call = process(exp.shift)
    "#{call}(&#{block})"
  end

  def process_call(exp)
    receiver_tree = exp.shift
    receiver_kind = receiver_tree[0]
    receiver = process(receiver_tree)
    case receiver_kind
    when :rescue, :lasgn, :iasgn, :masgn, :attr_asgn, :block, :op_asgn_or then
      receiver = "(" + receiver + ")"
    end
    name = exp.shift
    args_exp = exp.shift
    if args_exp && args_exp.first == :array
      args = "#{process(args_exp)[1..-2]}"
    else
      args = process args_exp
    end

    case name
    when :<=>, :==, :<, :>, :<=, :>=, :-, :+, :*, :/, :% then
      "(#{receiver} #{name} #{args})"
    when :[] then
      "#{receiver}[#{args}]"
    else
      "#{receiver}.#{name}#{args ? "(#{args})" : args}"
    end
  end

  def process_case(exp)
    s = "case #{process exp.shift}\n"
    until exp.empty?
      pt = exp.shift
      if pt.first == :when
        s << "#{process(pt)}\n"
      else
        s << "else\n#{indent(process(pt))}\n"
      end
    end
    s + "\nend"
  end

  def process_class(exp)
    s = "class #{exp.shift} < #{exp.shift}\n"
    body = ""
    body << "#{process exp.shift}\n\n" until exp.empty?
    s + indent(body) + "end"
  end

  def process_const(exp)
    exp.shift.to_s
  end

  def process_colon2(exp)
    "#{process(exp.shift)}::#{exp.shift.to_s}"
  end

  def process_dasgn_curr(exp)
    var_name = exp.shift.to_s
    if exp.empty?
      var_name
    else
      var_name + " = " + process(exp.shift)
    end
  end

  def rewrite_defn(exp)
    exp.shift # :defn
    name = exp.shift
    args = s(:args)
    body = Sexp.from_array exp.shift

    case body.first
    when :scope, :fbody then
      body = body[1] if body.first == :fbody
      args = body.last[1]
      assert_type args, :args
      assert_type body, :scope
      assert_type body[1], :block
      body.last.delete_at 1
    when :bmethod then
      # BEFORE: [:defn, :bmethod_added, [:bmethod, [:dasgn_curr, :x], ...]]
      # AFTER:  [:defn, :bmethod_added, [:args, :x], [:scope, [:block, ...]]]
      body.shift # :bmethod
      # [:dasgn_curr, :x],
      # [:call, [:dvar, :x], :+, [:arglist, [:lit, 1]]]]]
      dasgn = body.shift
      assert_type dasgn, :dasgn_curr
      dasgn.shift # type
      args.push(*dasgn)
      body.find_and_replace_all(:dvar, :lvar)
      if body.first.first == :block then
        body = s(:scope, body.shift)
      else
        body = s(:scope, s(:block, body.shift)) # single statement
      end
    when :dmethod
      # BEFORE: [:defn, :dmethod_added, [:dmethod, :bmethod_maker, ...]]
      # AFTER:  [:defn, :dmethod_added, ...]
      body = body[2][1][2] # UGH! FIX
      args = body[1]
      body.delete_at 1
      body = s(:scope, body)
    when :ivar then
      body = s(:scope, s(:block, s(:return, body)))
    when :attrset then
      argname = body.last
      args << :arg
      body = s(:scope, s(:block, s(:return, s(:iasgn, argname, s(:lvar, :arg)))))
    else
      raise "Unknown :defn format: #{name.inspect} #{args.inspect} #{body.inspect}"
    end

    return s(:defn, name, args, body)
  end

  def process_defn(exp)
    name = exp.shift
    args = process(exp.shift).to_a
    body = indent(process(exp.shift))
    return "def #{name}#{args}\n#{body}end".gsub(/\n\s*\n+/, "\n")
  end

  def process_dot2(exp)
    "(#{process exp.shift}..#{process exp.shift})"
  end

  def process_dot3(exp)
    "(#{process exp.shift}...#{process exp.shift})"
  end

  def process_dstr(exp)
    s = exp.shift.dump[0..-2]
    until exp.empty?
      pt = exp.shift
      if pt.first == :str
        s << process(pt)[1..-2]
      else
        s << '#{' + process(pt) + '}'
      end
    end
    s + '"'
  end

  def process_dvar(exp)
    exp.shift.to_s
  end

  def process_false(exp)
    "false"
  end

  def process_fcall(exp)
    exp_orig = exp.deep_clone
    # [:fcall, :puts, [:array, [:str, "This is a weird loop"]]]
    name = exp.shift.to_s
    args = exp.shift
    code = []
    unless args.nil? then
      assert_type args, :array
      args.shift # :array
      until args.empty? do
        code << process(args.shift)
      end
    end
    return "#{name}(#{code.join(', ')})"
  end

  def process_for(exp)
    collection = process(exp.shift)
    variables = process(exp.shift)
    body = process(exp.shift)
    "for " + variables + " in (" + collection + ") do\n" + indent(body) + "\nend"
  end

  def process_hash(exp)
    code = []
    until exp.empty? do
      code << [process(exp.shift), process(exp.shift)]
    end
    return "{" + code.map{|hp| "#{hp[0]} => #{hp[1]}"}.join(", ") + "}"
  end

  def process_iasgn(exp)
    "#{exp.shift} = #{process exp.shift}"
  end

  def process_op_asgn_or(exp)
    "#{process exp.shift} ||= (#{process exp.shift})"
  end

  def process_if(exp)
    if exp.length==3 && exp[1].nil? && !exp[2].nil?
      s = ["unless (#{process exp.shift})"]
      exp.shift
      s << "#{indent(process(exp.shift))}"
    else
      s = ["if (#{process exp.shift})"]
      s << "#{indent(process(exp.shift))}"
      until exp.empty?
        branch = exp.shift
        s << "else\n#{indent(process(branch))}" if branch
      end
    end
    s << "end"
    s.join("\n")
  end

  def process_iter(exp)
    start = "#{process exp.shift} do |#{process exp.shift}|\n"
    body = exp.shift
    if body[0] == :block && body[1][0] == :dasgn_curr && (body[1][2] == nil || body[1][2][0] == :dasgn_curr)
      body.delete_at(1)
    end
    start + indent("#{process body}\n") + "end"
  end

  def process_ivar(exp)
    exp.shift.to_s
  end

  def process_lasgn(exp)
    variable = exp.shift
    if exp.empty?
      variable.to_s
    else
      "#{variable} = #{process exp.shift}"
    end
  end

  def process_lit(exp)
    obj = exp.shift
    if obj.is_a? Range # to get around how parsed ranges turn into lits and lose parens
      "(" + obj.inspect + ")"
    else
      obj.inspect
    end
  end

  def process_lvar(exp)
    exp.shift.to_s
  end

  def process_gvar(exp)
    exp.shift.to_s
  end

  def process_masgn(exp)
    lhs = exp.shift
    raise "Not an array: #{lhs}" unless lhs.first == :array
    result = arg_list(lhs.sexp_body)
    unless exp.empty?
      result << " = "
      rhs = exp.shift
      if rhs.first == :array
        result << arg_list(rhs.sexp_body)
      else
        result << process(rhs)
      end
    end
    result
  end

  def process_svalue(exp)
    arg_list(exp.shift.sexp_body)
  end

  def process_to_ary(exp)
    process(exp.shift)
  end

  def process_match2(exp)
    left = process(exp.shift)
    right = process(exp.shift)
    "(#{left} =~ #{right})"
  end

  def process_match3(exp)
    right = process(exp.shift)
    left = process(exp.shift)
    "(#{left} =~ #{right})"
  end

  def process_next(exp)
    "next"
  end

  def process_nil(exp)
    "nil"
  end

  def process_return(exp)
    "return #{process exp.shift}"
  end

  def process_yield(exp)
    formatted_params = ""
    if args = exp.shift
      if args[0] == :array
        args.shift
        code = []
        until args.empty? do
          code << process(args.shift)
        end
        formatted_params = "(" + code.join(", ") + ")"
      else
        formatted_params = "(" + process(args) + ")"
      end
    end
    "yield#{formatted_params}"
  end

  def process_scope(exp)
    process(exp.shift)
  end

  def process_self(exp)
    "self"
  end
  def process_str(exp)
    exp.shift.dump
  end

  def process_super(exp)
    "super(#{process(exp.shift)})"
  end

  def process_true(exp)
    "true"
  end

  def process_until(exp)
    cond_loop(exp, 'until')
  end

  def process_vcall(exp)
    return exp.shift.to_s
  end

  def process_when(exp)
    "when #{process(exp.shift).to_s[1..-2]}\n#{indent(process(exp.shift))}"
  end

  def process_while(exp)
    cond_loop(exp, 'while')
  end

  def process_zarray(exp)
    "[]"
  end

  def process_zsuper(exp)
    "super"
  end

  def process_rescue(exp)
    expr = exp.shift
    body = exp.shift
    if expr[0] == :block
      "(\n" + indent(process(expr) + "\n) ") + process(body)
    else
      process(expr) + " " + process(body)
    end
  end

  def process_resbody(exp)
    first_child = exp.shift
    if first_child[0] == :block
      "rescue (\n" + indent(process(first_child) + "\n) ")
    elsif first_child[0] == :array && !exp.empty?
      # rescue Ex1, ..., Exn => var
      #  .....
      second_child = exp.shift
      first_child.shift # remove :array
      exceptions = []
      until first_child.empty?
        exceptions << process(first_child.shift)
      end
      "\nrescue " + exceptions.join(', ') + "\n" + indent(process(second_child)) + "\n" +
        (exp.empty? ? "" :  process(exp.shift))
    else
      "rescue " + process(first_child)
    end
  end

  def process_begin(exp)
    "begin\n" + indent(process(exp.shift)) + "\nend"
  end

  def cond_loop(exp, name)
    cond = process(exp.shift)
    body = indent(process(exp.shift))
    head_controlled = exp.empty? ? false : exp.shift

    code = []
    if head_controlled then
      code << "#{name} #{cond} do"
      code << body
      code << "end"
    else
      code << "begin"
      code << body
      code << "end #{name} #{cond}"
    end
    code.join("\n")
  end

  def arg_list(exp, delim = ", ")
    code = []
    while !exp.empty?
      code << process(exp.shift)
    end
    code.join delim
  end

end

if __FILE__ == $0
  r2r2r = RubyToRuby.translate(RubyToRuby).sub("RubyToRuby","RubyToRubyToRuby")
  eval r2r2r

  class RubyToRubyToRuby
    class<<self
      eval RubyToRuby.translate(class<<RubyToRuby;self;end, :translate)
    end
    eval RubyToRuby.translate(RubyToRuby, :initialize)
  end

  r2r2r2 = RubyToRubyToRuby.translate(RubyToRuby).sub("RubyToRuby","RubyToRubyToRuby")
  r2r2r2r = RubyToRubyToRuby.translate(RubyToRubyToRuby)
  # File.open('1','w'){|f| f.write r2r2r}
  # File.open('2','w'){|f| f.write r2r2r2}
  # File.open('3','w'){|f| f.write r2r2r2r}
  raise "Translation failed!" if (r2r2r != r2r2r2) or (r2r2r != r2r2r2r)

  puts("RubyToRubyToRubyToRubyyyyy!!!")
end
