unless $:.include?(File.dirname(__FILE__) + "/lib")
  $:.unshift(File.dirname(__FILE__) + "/lib")
end
require "rails_patches_for_template_optimizer"
require "template_optimizer"
