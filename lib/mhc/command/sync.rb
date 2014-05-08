module Mhc
  module Command
    class Sync
      Encoding.default_external = "UTF-8"

      def initialize(channel_name)
        config  = Mhc::Config::Sync.create_from_yaml_file("~/.mhc/config.yml")
        channel = config.sync_channels[channel_name]

        unless channel
          STDERR.print "No sync channel definition about #{channel_name} was found in ~/.mhc/config.yml"
          exit 1
        end

        builder = Mhc::Builder.new(config)

        driver = builder.sync_driver(channel.name, channel.strategy)
        driver.sync_all
      end

    end # class Sync
  end # module Command
end # module Mhc
