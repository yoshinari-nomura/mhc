require "bundler/gem_tasks"
require "rspec/core/rake_task"

RSpec::Core::RakeTask.new(:spec)

task :default => :spec

require 'rdoc/task'
Rake::RDocTask.new do |rd|
  rd.rdoc_dir = 'rdocs'
  rd.rdoc_files = FileList["lib/**/*.rb"]
  rd.options << '-charset=UTF-8'
end

task :release => :check_version
task :build => :check_version

# Emacs stuffs must have the same version numbers
# with Mhc::VERSION.
task :check_version do
  for file in %w(mhc-vars.el Cask)
    path = File.expand_path("../emacs/#{file}", __FILE__)
    raise "File not found #{path}" unless File.exists?(path)

    if /(\d+\.\d+\.\d+).*MHC_VERSION/ !~ File.open(path).read || Mhc::VERSION != $1
      raise "#{path} does not have valid version number (#{$1})."
    end
  end
end
