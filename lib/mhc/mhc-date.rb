# -*- coding: utf-8 -*-
### mhc-date.rb
##
## Author:  Yoshinari Nomura <nom@quickhack.net>
##
## Created: 1999/07/16
## Revised: $Date: 2004/10/25 02:28:57 $
##

require 'mhc-kconv'

class MhcTime
  include Comparable

  def initialize(h = 0, m = 0)
    if h .is_a?(String) && h =~ /^(\d+):(\d+)$/
      @sec = ($1 .to_i) * 3600 + ($2 .to_i) * 60
    else
      @sec = (h .to_i)  * 3600 + (m .to_i)  * 60
    end
  end

  def day;      @sec          / 86400 ;end
  def hour;    (@sec % 86400) / 3600  ;end
  def minute;  (@sec % 3600)  / 60    ;end

  def hh;   @sec / 3600        ;end
  def mm;  (@sec % 3600) / 60  ;end

  def <=>(o)
    if o .kind_of?(MhcTime)
      return @sec <=> o .to_i
    else
      return nil
    end
  end

  def to_s
    return format("%02d:%02d", hh, mm)
  end

  def to_i
    return @sec
  end

  def to_a
    return [hh, mm]
  end

  def to_t(date = MhcDate .new(1970, 1, 2))
    date = date .succ(day)
    Time .local(date .y, date .m, date .d, hour, minute)
  end
end

################################################################
## MhcDate class

class MhcDate
  include Comparable

  D_TABLE  = [0, 306, 337, 0, 31, 61, 92, 122, 153, 184, 214, 245, 275]
  O_LABEL  = %w(1st 2nd 3rd 4th 5th Last)
  M_LABEL  = %w(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)
  W_LABEL  = %w(Sun Mon Tue Wed Thu Fri Sat)
  W_JLABEL = %w(日 月 火 水 木 金 土)

  M_LONG_LABEL = %w(January February March April May June July August September October November December)
  W_LONG_LABEL = %w(Sunday Monday Tuesday Wednesday Thursday Friday Saturday)

  def initialize(y = -1, m = 1, d = 1)
    if y .kind_of?(String) && y =~ /^(\d{4})(\d\d)(\d\d)$/
      @y, @m, @d = $1 .to_i, $2 .to_i, $3 .to_i
    else
      if (y == -1)
        t = Time .now
        @y, @m, @d = t .year, t .month, t .day
      else
        @y, @m, @d = y, m, d
      end
    end
  end

  attr :y
  attr :m
  attr :d
  def w;     return (days + 4) % 7                    ; end
  def o;     return (@d - 1) / 7                      ; end

  def ym_a;  return [@y, @m]                          ; end
  def md_a;  return [@m, @d]                          ; end
  def to_a;  return [@y, @m, @d]                      ; end

  #def to_t(hh, mm);  return Time .local(@y, @m, @d, hh, mm); end

  def to_t(tim = MhcTime .new(0, 0))
    return Time .local(@y, @m, @d, tim .hour, tim .minute) + (tim .day * 86400)
  end

  ## X-SC- で使われる表現形式
  def y_s;   format("%04d", @y)                 ; end
  def m_s;   M_LABEL[@m - 1]                    ; end
  def d_s;   format("%02d", @d)                 ; end
  def w_s;   W_LABEL[w]                         ; end
  def o_s;   O_LABEL[o]                         ; end
  def to_s;  format("%04d%02d%02d", @y, @m, @d) ; end

  ## できるだけ数字で表す表現形式
  alias y_s1            y_s
  def   m_s1 (s = '');  format("%02d", @m)                        ; end
  def   d_s1 (s = '');  format("%02d", @d)                        ; end
  def   ym_s1(s = '');  format("%04d#{s}%02d", @y, @m)            ; end
  def   md_s1(s = '');  format("%02d#{s}%02d", @m, @d)            ; end
  def   to_s1(s = '');  format("%04d#{s}%02d#{s}%02d", @y, @m, @d); end
  #alias inspect to_s1

  ## できるだけ人間に分かりやすい表現形式
  if ENV['LANG'] =~ /^ja/i
    def ym_js
      MhcKconv::todisp(format("%04d年%02d月", @y, @m))
    end
    def md_js
      MhcKconv::todisp(format("%02d月%02d日(%s)", @m, @d, W_JLABEL[w]))
    end
    def to_js
      MhcKconv::todisp(format("%04d年%02d月%02d日(%s)",
                              @y, @m, @d, W_JLABEL[w]))
    end
  else
    def ym_js; format("%s %d", m_s, @y)                             ; end
    def md_js; format("%s, %d %s", w_s, @d, m_s)                    ; end
    def to_js; format("%s, %d %s %d", w_s, @d, m_s, @y)             ; end
  end
  def w_js;    W_LABEL[w]                                           ; end

  ################
  ## year
  def leap?
    return true if (@y % 4 == 0 and @y % 100 != 0) or @y % 400 == 0
    return false
  end

  def leap
    return leap? ? 1 : 0
  end

  def y_succ!(n = 1)
    @y += n
    return self
  end

  def y_succ(n = 1)
    return MhcDate .new(@y, @m, @d) .y_succ!(n)
  end

  ################
  ## order
  def o_last?
    return @d > m_days - 7
  end

  ################
  ## month
  def m_days
    return [31, 28 + leap, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][@m - 1]
  end

  def m_succ!(n = 1)
    months = (@y - 1) * 12 + @m + n
    @y = (months - 1) / 12 + 1
    @m = (months - 1) % 12 + 1
    @d = 1
    return self
  end

#    def m_succ!(n = 1)
#      xx = @m + n
#      pp = 0 < xx ? (xx - 1) / 12 : (xx - 12) / 12
#      @y += pp
#      @m = xx - (pp * 12)
#      @d = 1
#      return self
#    end

  def m_first_day
    return MhcDate .new(@y, @m, 1)
  end

  def m_last_day
    return MhcDate .new(@y, @m, m_days)
  end

  def m_succ(n = 1)
    return MhcDate .new(@y, @m, @d) .m_succ!(n)
  end

  def m_each_day
    for i in (1 .. m_days)
      dd = MhcDate .new(@y, @m, i)
      yield dd
    end
  end

  ################
  ## week
  def w_this(week_str_or_num = self .w)
    if week_str_or_num .kind_of?(String)
      week_number = W_LABEL .index(week_str_or_num[0,3] .capitalize)
    else
      week_number = week_str_or_num
    end
    return succ(week_number - w)
  end

  def w_first_day
    ## xxx currently, 0 means Sunday,
    ## but definition of `the first day of a week' should be able to be changed.
    return w_this(0)
  end

  def w_last_day
    ## xxx currently, 6 means Saturday,
    ## but definition of `the last day of a week' should be able to be changed.
    return w_this(6)
  end

  ################
  ## date

  ## xxx: succ and dec are very stupid.
  def succ!(n = 1)
    if (n < 0)
      dec!(- n)
    else
      for i in (1 .. n)
        if @d == m_days
          @d = 1
          m_succ!(1)
        else
          @d += 1
        end
      end
    end
    return self
  end

  def succ(n = 1)
    return MhcDate .new(@y, @m, @d) .succ!(n)
  end

  def dec!(n = 1)
    if (n < 0)
      succ!(- n)
    else
      for i in (1 .. n)
        if @d == 1
          m_succ!(-1)
          @d = m_days
        else
          @d -= 1
        end
      end
    end
    return self
  end

  def dec(n = 1)
    return MhcDate .new(@y, @m, @d) .dec!(n)
  end

  def days
    yy = @m < 3 ? @y - 1 : @y
    return yy * 365 + yy / 4 - yy / 100 + yy / 400 + D_TABLE[@m] + @d - 719469
  end

  def hash
    return (@y << 9) + (@m << 5) + @d
  end
  ## alias hash days

  def today?
    t = Time .now
    return ((@y == t .year) and (@m == t .month) and (@d == t .day))
  end

  def <=>(other)
    if other .kind_of?(MhcDate)
      return days <=> other .days
    else
      return nil
    end
  end

  def eql?(other)
    return @d == other .d && @m == other .m && @y == other .y
  end
  ## alias eql? ==

  def -(other)
    return days - other .days
  end
end

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

### mhc-date.rb ends here
