# -*- ruby -*-

require 'rubygems'
require 'rake'
require 'rake/testtask'
require 'rake/rdoctask'

desc "Default Task"
task :default => [ :rdoc ]

desc "Cleaning"
task :clean do
  sh "rm -rf html"
  sh "find . -name '*~' | xargs rm -f"
end

desc "Packaging"
task :package do
  sh "rm -rf /tmp/template_optimizer template_optimizer.tar.gz"
  sh "svn export . /tmp/template_optimizer"
  sh "tar czvCf /tmp template_optimizer.tar.gz template_optimizer"
  sh "rm -rf /tmp/template_optimizer"
end


# Run the unit tests

Rake::TestTask.new { |t|
  t.libs << "test"
  t.test_files=Dir.glob( "*_test.rb" )
  t.verbose = false
}

# Create documentation

Rake::RDocTask.new { |r|
  # rd.main = "README.doc"
  # rd.rdoc_files.include("README.rdoc", "lib/**/*.rb")
  r.rdoc_files.include("README", "lib/template_optimizer.rb")
  r.options.concat %w(--all --inline-source)
}


