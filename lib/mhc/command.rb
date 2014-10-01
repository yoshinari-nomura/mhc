module Mhc
  module Command

    dir = File.dirname(__FILE__) + "/command"

    autoload :Completions, "#{dir}/completions.rb"
    autoload :Cache,       "#{dir}/cache.rb"
    autoload :Init,        "#{dir}/init.rb"
    autoload :Scan,        "#{dir}/scan.rb"
    autoload :Sync,        "#{dir}/sync.rb"

  end # module Command
end # module Mhc
