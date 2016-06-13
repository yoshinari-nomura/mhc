module Mhc
  class Formatter
    class Mail < Text
      private

      def format_header(context)
        mail_address = context[:mailto].to_s
        subject = format("MHC schedule report (%s--%s)", *context[:items].keys.minmax)
        header =  "To: #{mail_address}\n"
        header += "From: #{append(mail_address, "secretary-of-")}\n"
        header += "Subject: #{subject}\n"
        header += "Content-Type: Text/Plain; charset=utf-8\n"
        header += "Content-Transfer-Encoding: 8bit\n"
        header += "\n"
        header += format("* mhc %s--%s\n", *context[:items].keys.minmax)
      end

    end # class Mail
  end # class Formatter
end # module Mhc
