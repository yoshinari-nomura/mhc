module Mhc
  module Command
    class Scan
      Encoding.default_external = "UTF-8"

      def initialize(calendar, range_string, format: :text, search: nil, **options)
        formatter = Mhc::Formatter.build(formatter: format, **options)
        date_range = Mhc::PropertyValue::Date.parse_range(range_string)
        format_range(calendar, formatter, date_range, search: search, **options)
      end

      def format_range(calendar, formatter, date_range, search: nil, **options)
        if search
          begin
            search_proc = Mhc::Query.new(search).to_proc
          rescue Mhc::Query::ParseError => e
            STDERR.print "Error: " + e.message + " in search option.\n"
            exit 1
          end
        end

        calendar.scan(date_range, &search_proc).each do |date, items|
          formatter << [date, items]
        end
        print formatter.to_s
      end
    end # class Scan
  end # module Command
end # module Mhc

### Copyright Notice:

## Copyright (C) 1999, 2000 Yoshinari Nomura. All rights reserved.
## Copyright (C) 2000 MHC developing team. All rights reserved.

## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
##
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
## 3. Neither the name of the team nor the names of its contributors
##    may be used to endorse or promote products derived from this software
##    without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS''
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
## FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
## THE TEAM OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
## INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
## HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
## STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
## ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
## OF THE POSSIBILITY OF SUCH DAMAGE.
