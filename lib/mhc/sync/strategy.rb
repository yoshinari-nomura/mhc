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
      #                    Side 2
      #   |---+---------+------------+------------+-------|
      # S |   | M       | U          | N          | D     |
      # i |---+---------+------------+------------+-------|
      # d | M | CNF     | OW 1->2    | CP 1->2    | CNF   |
      # e | U | OW 2->1 | -          | ?? CP 1->2 | DEL 1 |
      # 1 | N | CP 2->1 | ?? CP 2->1 | -          | -     |
      #   | D | CNF     | DEL 2      | -          | -     |
      #   |---+---------+------------+------------+-------|
      #
      #   M, U, N, and D indicate status changes on each article after
      #   the last sync:
      #
      #   + M :: Modified (or Created)
      #   + U :: Unchanged
      #   + N :: No Record
      #   + D :: Deleted
      #
      #   Each entry in the table means:
      #   + -- :: No operation (ignore)
      #   + ?? :: Not occurred in normal cases
      #   + OW :: Overwrite
      #   + CP :: Copy
      #   + DEL :: Delete
      #   + CNF :: Conflict
      #
      # Before applying the rule to our repository,
      # we have to set the marks (M, U, N or D) to all articles
      # in each side.
      #
      # strategy = Mhc::Sync::Strategy.create(strategy_name)
      # strategy name is one of:
      # * :empty  ... ignore on every status
      # * :mirror ... mirror from side1 to side2
      # * :sync   ... sync articles of side1 and side2
      #
      # and strategy.whatnow(side1, side2) returns a symbol one of:
      # * :ignore :: Already synced, ignoreable
      # * :conflict :: Conflicted
      # * :delete1 :: Should delete the article of side1
      # * :delete2 :: Should delete the article of side2
      # * :copy1_to_2 :: Should copy the article of side1 to side2
      # * :copy2_to_1 :: Should copy the article of side2 to side1
      # * :overwrite1_to_2 :: Should overwrite the article of side1 to side2
      # * :overwrite2_to_1 :: Should overwrite the article of side2 to side1
      #
      # side1 and side2 have to respond to:
      #     #nil?, # #modified?, #unmodified?, #norecord?, #deleted?
      #
      class Base
        def whatnow(side1, side2)
          # do nothing
          actions = {
            "MM" => :ignore,
            "MU" => :ignore,
            "MN" => :ignore,
            "MD" => :ignore,

            "UM" => :ignore,
            "UU" => :ignore,
            "UN" => :ignore,
            "UD" => :ignore,

            "NM" => :ignore,
            "NU" => :ignore,
            "NN" => :ignore,
            "ND" => :ignore,

            "DM" => :ignore,
            "DU" => :ignore,
            "DN" => :ignore,
            "DD" => :ignore,
          }
          return actions[status_pair(side1, side2)]
        end

        private
        # * Char (M,U,N,D) indicates status change on each article
        #   after the last sync:
        #
        #   + M :: Modified
        #   + U :: Unchanged
        #   + N :: No Record
        #   + D :: Deleted
        #
        def status_signature(info)
          return "N" if info.nil?

          return "M" if info.modified? || info.created?
          return "U" if info.unmodified?
          return "N" if info.norecord?
          return "D" if info.deleted?

          return "?" # NOTREACHED I hope
        end

        def status_pair(side1, side2)
          return status_signature(side1) + status_signature(side2)
        end
      end # class Base

      # * Sync side1 and side2
      #
      #   simply follow the rule on the table below:
      #
      #                    Side 2
      #   |---+---------+------------+------------+-------|
      # S |   | M       | U          | N          | D     |
      # i |---+---------+------------+------------+-------|
      # d | M | CNF     | CP 1->2    | CP 1->2    | CNF   |
      # e | U | CP 2->1 | -          | ?? -       | DEL 1 |
      # 1 | N | CP 2->1 | ?? -       | -          | -     |
      #   | D | CNF     | DEL 2      | -          | -     |
      #   |---+---------+------------+------------+-------|
      #
      #   + M :: Modified (or Created)
      #   + U :: Unchanged
      #   + N :: No Record
      #   + D :: Deleted
      #
      #   + -- :: No operation (ignore)
      #   + ?? :: Not occurred in normal cases
      #   + OW :: Overwrite
      #   + CP :: Copy
      #   + DEL :: Delete
      #   + CNF :: Conflict
      #
      class Sync < Base
        def whatnow(side1, side2)
          actions = {
            "MM" => :conflict,
            "MU" => :copy1_to_2,
            "MN" => :copy1_to_2,
            "MD" => :conflict,

            "UM" => :copy2_to_1,
            "UU" => :ignore,
            "UN" => :ignore,
            "UD" => :delete1,

            "NM" => :copy2_to_1,
            "NU" => :ignore,
            "NN" => :ignore,
            "ND" => :ignore,

            "DM" => :conflict,
            "DU" => :delete2,
            "DN" => :ignore,
            "DD" => :ignore,
          }
          return actions[status_pair(side1, side2)]
        end
      end # class Sync

      # * Mirror side1 to side2
      #
      #   simply follow the rule on the table below:
      #
      #                    Side 2
      #   |---+---------+----------+------------+---------|
      # S |   | M       | U        | N          | D       |
      # i |---+---------+----------+------------+---------|
      # d | M | OW 1->2 | OW 1->2  | CP 1->2    | CP 1->2 |
      # e | U | OW 1->2 | --       | ?? --      | CP 1->2 |
      # 1 | N | DEL 2   | ?? --    | --         | --      |
      #   | D | DEL 2   | DEL 2    | --         | --      |
      #   |---+---------+----------+------------+---------|
      #
      #   + M :: Modified (or Created)
      #   + U :: Unchanged
      #   + N :: No Record
      #   + D :: Deleted
      #
      #   + -- :: No operation (ignore)
      #   + ?? :: Not occurred in normal cases
      #   + OW :: Overwrite
      #   + CP :: Copy
      #   + DEL :: Delete
      #
      class Mirror < Base
        def whatnow(side1, side2)
          actions = {
            "MM" => :overwrite1_to_2,
            "MU" => :overwrite1_to_2,
            "MN" => :copy1_to_2,
            "MD" => :copy1_to_2,

            "UM" => :overwrite1_to_2,
            "UU" => :ignore,
            "UN" => :ignore,
            "UD" => :copy1_to_2,

            "NM" => :delete2,
            "NU" => :ignore,
            "NN" => :ignore,
            "ND" => :ignore,

            "DM" => :delete2,
            "DU" => :delete2,
            "DN" => :ignore,
            "DD" => :ignore,
          }
          return actions[status_pair(side1, side2)]
        end
      end # class Mirror

    end # module Strategy
  end # module  Sync
end # module Mhc
