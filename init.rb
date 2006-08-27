unless $:.include?(File.dirname(__FILE__) + "/lib")
  $:.unshift(File.dirname(__FILE__) + "/lib")
  require "template_optimizer"
end

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
  @@template_log_dir = "#{RAILS_ROOT}/tmp/templates"
  
  def simple_cache_key(str)
    "#{request_host}/#{str}"
  end
  
  def request_host
    @request_host ||= @controller.request.host
  end
 
  private
  
  def log_template_compilation(method_key, render_symbol, render_source)
    logger.debug "Compiled template #{method_key}\n  ==> #{render_symbol}" if logger
    if self.class.log_template_source
      File.open("#{@@template_log_dir}/#{render_symbol}.src", "w") do |f|
        f.puts render_source
      end
    end
  end
  
  def optimize_template_code(render_symbol, extension)
    if self.class.optimize_templates
      if TemplateOptimizer.ignore_method?(render_symbol) || extension.to_s != "rhtml"
        logger.info "TO: ignored optimization of render method #{render_symbol}" if logger
      else
        begin
          TemplateOptimizer.new(CompiledTemplates, render_symbol, self,
                                "#{@@template_log_dir}/#{render_symbol}.rb",
                                logger).optimize
          logger.info "TO: optimized render method #{render_symbol}" if logger
          rescue Object => e
           if logger
             logger.error "TO: optimizing #{render_symbol} RAISED #{e}"
             logger.error "TO: Backtrace: #{e.backtrace.join("\nTO: ")}\n"
           end
        end
      end
    end
  end
  
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

    optimize_template_code(render_symbol, extension)

    @@compile_time[render_symbol] = Time.now
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

class ::Object
  def to_param
    to_s
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
    
    end
  
    unless defined? ::ActionController::CodeGeneration
      class Route
        def generation_requirements
          requirement_conditions = requirements.collect do |key, req|
            if req.is_a? Regexp
              value_regexp = Regexp.new "\\A#{req.source}\\Z"
              conditional_regexp = "Routing.ignore_regexps ? /.+/ : #{value_regexp.inspect}"
              "hash[:#{key}] && (#{conditional_regexp}) =~ options[:#{key}]"
            else
              "hash[:#{key}] == #{req.inspect}"
            end
          end
          requirement_conditions * ' && ' unless requirement_conditions.empty?
        end
      end
    end
    
  end # Routing
end # ActionController

::ActionController::Routing::Routes.reload
