class ::ActionView::Base

  # turn on optimization by setting this option
  cattr_accessor :optimize_templates
  @@optimize_templates = ARGV.include?('-OT')

  # warning: this will slow down the optimization process considerably
  cattr_accessor :log_template_optimization
  @@log_template_optimization = ARGV.include?('-Ol')

  # if this options is set, the Erb source code will be logged
  cattr_accessor :log_template_source
  @@log_template_source = ARGV.include?('-Ot')

  # specify directory for logging
  cattr_accessor :template_log_dir
  @@template_log_dir = "#{defined?(RAILS_ROOT) ? RAILS_ROOT : ENV['HOME']}/tmp/templates"

  def simple_cache_key(str)
    "#{request_host}/#{str}"
  end

  def request_host
    @request_host ||= @controller.request.host
  end

  private

  def log_template_compilation(method_key, render_symbol, render_source)
    logger.debug "Compiled template #{method_key}\n  ==> #{render_symbol}" if logger
    if @@log_template_source
      File.open("#{@@template_log_dir}/#{render_symbol}.src", "w") do |f|
        f.puts render_source
      end
    end
  end

  def optimize_template_code(render_symbol)
    if @@optimize_templates
      if TemplateOptimizer.ignore_method?(render_symbol) || render_symbol.to_s !~ /^_run_(rhtml|erb)_/
        logger.info "TO: ignored optimization of render method #{render_symbol}" if logger
      else
        begin
          optimizer = TemplateOptimizer.new(CompiledTemplates, render_symbol, self,
                                            "#{@@template_log_dir}/#{render_symbol}.rb",
                                            logger)
          optimizer.optimize
          logger.info "TO: optimized render method #{render_symbol}" if logger
          rescue Object => e
           if logger
             logger.error "TO: optimizing #{render_symbol} RAISED #{e}"
             logger.error "TO: current optimizer pass: #{optimizer.current_pass}"
             logger.error "TO: current optimizer iteration: #{optimizer.current_iteration}"
             logger.error "TO: backtrace: #{e.backtrace.join("\nTO: ")}\n"
           end
        end
      end
    end
  end

  if private_instance_methods.include?("find_base_path_for") # rails 2.x
    def compile_template(handler, template, file_name, local_assigns)
      method_key = file_name || template
      render_symbol = assign_method_name(handler, template, file_name)
      render_source = create_template_source(handler, template, render_symbol, local_assigns.keys)
      line_offset   = @@template_args[render_symbol].size + handler.line_offset

      begin
        file_name = 'compiled-template' if file_name.blank?
        CompiledTemplates.module_eval(render_source, file_name, -line_offset)
      rescue Exception => e  # errors from template code
        if logger
          logger.debug "ERROR: compiling #{render_symbol} RAISED #{e}"
          logger.debug "Function body: #{render_source}"
          logger.debug "Backtrace: #{e.backtrace.join("\n")}"
        end

        raise TemplateError.new(extract_base_path_from(file_name) || view_paths.first, file_name || template, @assigns, template, e)
      end

      log_template_compilation(method_key, render_symbol, render_source)
      optimize_template_code(render_symbol)

      @@compile_time[render_symbol] = Time.now
      # logger.debug "Compiled template #{file_name || template}\n  ==> #{render_symbol}" if logger
    end
  else # rails 1.x
    def compile_template(extension, template, file_name, local_assigns)
      method_key = file_name || template

      render_symbol = @@method_names[method_key] || assign_method_name(extension, template, file_name)
      render_source = create_template_source(extension, template, render_symbol, local_assigns.keys)

      line_offset = @@template_args[render_symbol].size
      if extension
        case extension.to_sym
        when :rxml, :rjs
          line_offset += 2
        end
      end

      begin
        unless file_name.blank?
          CompiledTemplates.module_eval(render_source, file_name, -line_offset)
        else
          CompiledTemplates.module_eval(render_source, 'compiled-template', -line_offset)
        end
      rescue Object => e
        if logger
          logger.debug "ERROR: compiling #{render_symbol} RAISED #{e}"
          logger.debug "Function body: #{render_source}"
          logger.debug "Backtrace: #{e.backtrace.join("\n")}"
        end

        raise TemplateError.new(@base_path, method_key, @assigns, template, e)
      end

      log_template_compilation(method_key, render_symbol, render_source)
      optimize_template_code(render_symbol)

      @@compile_time[render_symbol] = Time.now
    end
  end

  if private_instance_methods.include? 'compiled_method_name_file_path_segment'
    # produce method names which are easier to parse for humans (edge rails)
    def compiled_method_name_file_path_segment(file_name)
      if file_name
        s = File.expand_path(file_name).clone
        s.sub!(/^#{Regexp.escape(File.expand_path(RAILS_ROOT))}\//, '') if defined?(RAILS_ROOT)
        s.sub!(/\.r(html|xml|js)$/,'')
        s.tr!('/:-', '_')
        s.gsub!(/([^a-zA-Z0-9_])/) { $1[0].to_s }
        s
      else
        (@@inline_template_count += 1).to_s
      end
    end
  else
    # fix bug in 1.1.x
    def assign_method_name(extension, template, file_name)
      method_name = '_run_'
      method_name << "#{extension}_" if extension

      if file_name
        file_path = File.expand_path(file_name).clone
        root_path = File.expand_path(RAILS_ROOT) if defined? RAILS_ROOT
        file_path.sub!(/^#{Regexp.escape(root_path)}\//, '') if root_path
        file_path.sub!(/\.r(html|xml|js)$/,'')
        file_path.tr!('/:-', '_')
        file_path.gsub!(/[^a-zA-Z0-9_]/){|s| s[0].to_s}
        method_name += file_path
      else
        @@inline_template_count += 1
        method_name << @@inline_template_count.to_s
      end

      @@method_names[file_name || template] = method_name.intern
    end
  end

end

class ::ActionView::Helpers::InstanceTag
  class << self
    def check_box_check(object, method, checked_value)
      'checked="checked"' if check_box_checked?(safe_object_value(object, method), checked_value)
    end

    def radio_button_check(object, method, checked_value)
      'checked="checked"' if radio_button_checked?(safe_object_value(object, method), checked_value)
    end

    def safe_object_value(object, method)
      object.send method rescue nil
    end

    # helper support for 1.1.6, edge rails already contains this code
    unless defined? check_box_checked?

      def check_box_checked?(value, checked_value)
        case value
        when TrueClass, FalseClass
          value
        when NilClass
          false
        when Integer
          value != 0
        when String
          value == checked_value
        else
          value.to_i != 0
        end
      end

      def radio_button_checked?(value, checked_value)
        value.to_s == checked_value.to_s
      end

    end #helper support

  end
end

if defined? ::ActionController::CodeGeneration

  # patch stable to support a to_param method for (all) objects. the
  # obvious method won't work reliably: sometimes Object::to_param gets
  # called insted of the AR one. I have no idea why.
  # for edge rails, this isn't necessary.

  # class ::Object
  #   def to_param
  #     # puts "Object::to_param called on #{self.inspect}"
  #     # puts "superclass = #{self.class.superclass}"
  #     to_s
  #   end
  # end

  [Numeric, String, Class, ActionController::Base].each do |klass|
    klass.class_eval "def to_param; to_s; end"
  end

  [FalseClass, TrueClass, NilClass].each do |klass|
    klass.class_eval "def to_param; self; end"
  end
end

module ::ActionController
  module Routing
    class << self
      cattr_accessor :ignore_regexps

      if defined? ::ActionController::CodeGeneration
        def test_condition(expression, condition)
          case condition
          when String then "(#{expression} == #{condition.inspect})"
          when Regexp then
            condition = Regexp.new("^#{condition.source}$") unless /^\^.*\$$/ =~ condition.source
            "((Routing.ignore_regexps && (/.+/ =~ #{expression})) || #{condition.inspect} =~ #{expression})"
          when Array then
            conds = condition.collect do |condition|
              cond = test_condition(expression, condition)
              (cond[0, 1] == '(' && cond[-1, 1] == ')') ? cond : "(#{cond})"
            end
            "(#{conds.join(' || ')})"
          when true then expression
          when nil then "! #{expression}"
          else
            raise ArgumentError, "Valid criteria are strings, regular expressions, true, or nil"
          end
        end
      end

    end # class << self

    unless defined? ::ActionController::CodeGeneration
      # puts "TO: preparing for new routes"
      # puts "Route is #{Route.inspect}"
      class Route
        def generation_requirements
          # puts "%%%%%%%% calling new generation requirements: #{requirements.inspect}"
          requirement_conditions = requirements.collect do |key, req|
            # puts "%%%%%% #{req.inspect}"
            if req.is_a? Regexp
              # puts "%%%% req= #{req.inspect}"
              value_regexp = Regexp.new "\\A#{req.source}\\Z"
              conditional_regexp = "(Routing.ignore_regexps ? /\\A.+\\Z/ : #{value_regexp.inspect})"
              "hash[:#{key}] && #{conditional_regexp} =~ options[:#{key}]"
            else
              "hash[:#{key}] == #{req.inspect}"
            end
          end
          # puts "requirement_conditions: #{requirement_conditions * ' && '}"
          requirement_conditions * ' && ' unless requirement_conditions.empty?
        end
      end
    end

    class DynamicSegment
      def value_check
        if default # Then we know it won't be nil
          "(Routing.ignore_regexps || #{value_regexp.inspect} =~ #{local_name})" if regexp
        elsif optional?
          # If we have a regexp check that the value is not given, or that it matches.
          # If we have no regexp, return nil since we do not require a condition.
          "#{local_name}.nil? || Routing.ignore_regexps || #{value_regexp.inspect} =~ #{local_name}" if regexp
        else # Then it must be present, and if we have a regexp, it must match too.
          "#{local_name} #{"&& (Routing.ignore_regexps || #{value_regexp.inspect} =~ #{local_name})" if regexp}"
        end
      end
    end

  end # Routing
end # ActionController

# ::ActionController::Routing::Routes.reload
