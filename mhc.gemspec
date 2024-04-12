# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
git = File.expand_path('../.git', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'mhc/version'

Gem::Specification.new do |spec|
  spec.name          = "mhc"
  spec.version       = Mhc::VERSION
  spec.authors       = ["Yoshinari Nomura"]
  spec.email         = ["nom@quickhack.net"]
  spec.summary       = %q{Message Harmonized Calendaring}
  spec.description   = %q{Message Harmonized Calendaring.}
  spec.homepage      = "http://www.quickhack.net/mhc"
  spec.license       = "BSD"

  spec.files         = if Dir.exist?(git)
                         `git ls-files -z`.split("\x0")
                       else
                         Dir['**/*']
                       end
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_runtime_dependency "thor",        ">= 1.2.0"
  spec.add_runtime_dependency "rexml",       ">= 3.2.4"
  spec.add_runtime_dependency "ri_cal",      ">= 0.8.8"
  spec.add_runtime_dependency "tzinfo",      ">= 1.2.2"
  spec.add_runtime_dependency "tzinfo-data", ">= 1.2015.4"
  spec.add_runtime_dependency "nkf"

  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"
end
