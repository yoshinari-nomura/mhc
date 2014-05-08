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
