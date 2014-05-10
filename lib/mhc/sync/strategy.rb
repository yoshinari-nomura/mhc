module Mhc
  module Sync
    module Strategy

      class Factory
        def self.create(strategy)
          case strategy.to_sym
          when :mirror
            return Mirror.new
          when :sync
            return Sync.new
          else
            raise NotImplementedError, "#{strategy} #{strategy.class}"
          end
        end
      end

      # Our Sync mechanism is very simple, because
      # we can assume every article is independent
      # with eath other. It will work well with
      # iCalendar basis articles.
      #
      # We simply follow the rule on the table below:
      #
      #                  Local
      #     +---+---------------------------------+
      #   R |   | D          M          N         |
      #   e +---+---------------------------------+
      #   m | D | -          CONFLICT   DELETE L  |
      #   o | M | CONFLICT   CONFLICT   PUT R->L  |
      #   t | N | DELETE R   PUT L->R   -         |
      #   e +---+---------------------------------+
      #
      #   D, M, and N indicate status changes on each article after the last sync:
      #
      #   + D ... Deleted
      #   + M ... Modified or Created
      #   + N ... No Cahnge or No Entry
      #
      # Before applying the rule to our repository,
      # we have to set the marks (D, M or N) to all articles
      # in each side.
      #
      # strategy = Mhc::Sync::Strategy.create(strategy_name)
      # strategy name is one of
      # * :empty  ... ignore on every status
      # * :mirror ... mirror from side1 to side2
      # * :sync   ... sync articles of side1 and side2
      #
      # strategy.whatnow(side1, side2)
      # one of:
      # :ignore:: Already synced ignoreable
      # :conflict:: Conflicted
      # :delete_side1:: Should delete the article of side1
      # :delete_side2:: Should delete the article of side2
      # :side1_to_side2:: Should copy the article of side1 to side2
      # :side2_to_side1:: Should copy the article of side2 to side1
      #
      # side1 and side2 have to respond to #unmodified?, #deleted?
      # side1 and side2 can be nil
      #
      class Base
        def whatnow(side1, side2)
          # do nothing
          actions = {
            "DD" => :ignore,
            "DM" => :ignore,
            "DN" => :ignore,
            "MD" => :ignore,
            "MM" => :ignore,
            "MN" => :ignore,
            "ND" => :ignore,
            "NM" => :ignore,
            "NN" => :ignore
          }
          return actions[status_pair(side1, side2)]
        end

        private
        # * Char (D,M,N) indicates status change on each article
        #   after the last sync:
        #
        #   + D :: Deleted
        #   + M :: Modified or Created
        #   + N :: No Cahnge or No Entry
        #
        def status_signature(info)
          return "N" if info.nil? or info.unmodified?
          return "D" if info.deleted?
          return "M"
        end

        def status_pair(side1, side2)
          return status_signature(side1) + status_signature(side2)
        end
      end # class Base


      # * Sync Side1 and Side2
      #   We simply follow the rule on the table below:
      #
      #                    Side2
      #     +---+---------------------------------+
      #   S |   | D          M          N         |
      #   i +---+---------------------------------+
      #   d | D | -          CONFLICT   DELETE 2  |
      #   e | M | CONFLICT   CONFLICT   PUT 1->2  |
      #   1 | N | DELETE 1   PUT 2->1   -         |
      #     +---+---------------------------------+
      class Sync < Base
        def whatnow(side1, side2)
          actions = {
            "DD" => :ignore,
            "DM" => :conflict,
            "DN" => :delete_side2,
            "MD" => :conflict,
            "MM" => :conflict,
            "MN" => :side1_to_side2,
            "ND" => :delete_side1,
            "NM" => :side2_to_side1,
            "NN" => :ignore
          }
          return actions[status_pair(side1, side2)]
        end
      end # class Sync


      # * Mirror side1 to side2
      #   We simply follow the rule on the table below:
      #
      #                    Side2
      #     +---+---------------------------------+
      #   S |   | D          M          N         |
      #   i +---+---------------------------------+
      #   d | D | -          DELETE 2   DELETE 2  |
      #   e | M | PUT 1->2   PUT 1->2   PUT 1->2  |
      #   1 | N | PUT 1->2   PUT 1->2   -         |
      #     +---+---------------------------------+
      class Mirror < Base
        def whatnow(side1, side2)
          actions = {
            "DD" => :ignore,
            "DM" => :delete_side2,
            "DN" => :delete_side2,
            "MD" => :ow_side1_to_side2,
            "MM" => :ow_side1_to_side2,
            "MN" => :ow_side1_to_side2,
            "ND" => :ow_side1_to_side2,
            "NM" => :ow_side1_to_side2,
            "NN" => :ignore
          }
          return actions[status_pair(side1, side2)]
        end
      end # class Mirror

    end # module Strategy
  end # module  Sync
end # module Mhc
