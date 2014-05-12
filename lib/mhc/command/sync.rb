module Mhc
  module Command
    class Sync
      Encoding.default_external = "UTF-8"

      def initialize(channel_name, config)
        channel = config.sync_channels[channel_name]

        unless channel
          STDERR.print "Error: Not found '#{channel_name}' in ~/.mhc/config.yml\n"
          exit 1
        end

        builder = Mhc::Builder.new(config)

        driver = builder.sync_driver(channel.name, channel.strategy)
        driver.sync_all
      end

    end # class Sync
  end # module Command
end # module Mhc
