module Mhc
  class Formatter
    class OrgTable < Base
      private

      def format_header(context)
        format("* mhc %s--%s\n", *context[:items].keys.minmax)
      end

      def format_item(context, date, item)
        # | No | Mission | Recurrence | Subject | Path | Date |
        format("  | %4d | %s | %s | %s | %s | [%04d-%02d-%02d%s] |\n",
               context[:number],
               item.mission_tag.to_s.toutf8,
               item.recurrence_tag.to_s.toutf8,
               item.subject.to_s.toutf8,
               item.path.to_s,
               date.year, date.month, date.mday,
               append(item.time_range.to_s))
      end

    end # class OrgTable
  end # class Formatter
end # module Mhc
