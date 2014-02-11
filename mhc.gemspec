# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'mhc/version'

Gem::Specification.new do |spec|
  spec.name          = "mhc"
  spec.version       = Mhc::VERSION
  spec.authors       = ["Yoshinari Nomura"]
  spec.email         = ["nom@quickhack.net"]
  spec.summary       = %q{Message Harmonized Calendaring}
  spec.description   = %q{Message Harmonized Calendaring.}
  spec.homepage      = ""
  spec.license       = "BSD"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.5"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"
end
