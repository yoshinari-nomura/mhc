# -*- coding: utf-8 -*-
### mhc-schedule.rb
##
## Author:  Yoshinari Nomura <nom@quickhack.net>
##
## Created: 1999/07/16
## Revised: $Date: 2008/10/08 03:22:37 $
##

################################################################
######## Classes for handling schedule articles. ###############
################################################################

require 'mhc-signal'
require 'mhc-palm'
require 'mhc-date'

################################################################
## MHC schedule item class
##
## subject
##      get X-SC-Subject: value or ''
## set_subject(aString)
##      set X-SC-Subject:
## location
##      get X-SC-Location: value or ''
## set_location(aString)
##      set X-SC-Location:
## day
##      returns active dates exist in X-SC-Day: (means drop !yyyymmdd)
##      The return value is  a array of MhcDate or []
##
## day_as_string
##     return a string same as X-SC-Day: or ''
##     (means !yyyymmdd might be involved)
##
## add_day(aMhcDate)
##      add active date to X-SC-Day:
##      add_day considers other X-SC-fields smartly.
##           if the date is designated for a exception, remove it.
##           if the date is encumbered by duration:, raise an error.
##           if the date is covered by cond:, do nothing.
##
## del_day(aMhcDate)
##      inactivate aMhcDate.
##      del day considers other X-SC-fields smartly.
##          if the schedule item does not occur on the date, do nothing.
##          if the date is covered by cond:, add a exception !yyyymmdd.
##
## exception
##      return inactive dates exist in X-SC-Day: (means drop yyyymmdd)
##      the return value is  a array of MhcDate.
##
## time
##      return a time range in a form of [aMhcTime_begin, aMhcTime_end] or nil
##
## time_as_string
##      return a time range in a form of "xx:xx-xx:xx" or ''
##
## time_b
## time_e
##      get a begin/end time or nil
##
## set_time(aMhcTime_b = nil , aMhcTime_e = nil)
##      set begin and end times.
##
## alarm
##      return X-SC-Alarm: value in second or nil.
##
## alarm_as_string
##      return X-SC-Alarm: in a form of "xx (minute|hour|day)" or ''
##
## set_alarm(aInteger)
##      set alarm value in second.
##      nil means 'no alarm.'
##
## rec_id
##      return X-SC-Record-Id: value in string
##
## set_rec_id
##      set record-id for schedule entry
##
################################################################
## category
##      return X-SC-Category: value as an array of String.
##
## category_as_string
##      return a string same as X-SC-Category:
##
## set_category(aString or [aString,..])
##      set X-SC-Category:
##      A space separated string or an array of string is allowed as an arg.
##
## add_category(String)
## del_category(String)
##      add/remove a category.
##
## cond
##      return X-SC-Cond: value as an array of String or []
##
## cond_as_string
##      return a string same as X-SC-Cond:
##
## cond_mon
## cond_ord
## cond_wek
## cond_num
##      return specified value of X-SC-Cond: as an array of String.
##          mon stands for month  => Jan .. Dec
##          ord stands for order  => 1st .. Last
##          wek stands for week   => Sun .. Sat
##          num stands for number => 01 .. 31
##
## set_cond(String or [String,..])
##      set X-SC-Cond:
##      A space separated string or an array of string is allowed as an arg.
##
## add_cond(String)
## del_cond(String)
##      add/remove a cond.
##
## duration -> [aMhcDate_begin, aMhcDate_end] or nil
## duration_as_string -> aString "yyyymmdd-yyyymmdd" or ''
## duration_b -> aMhcDate_begin or nil
## duration_e -> aMhcDate_end or nil
## set_duration(aMhcDate_begin = nil, aMhcDate_end = nil)
##
## description -> aString
## set_description(aString)
##
## priority -> aInteger
## priority_as_string -> aString
## set_priority(aInteger)
##
## pilot_flag()
## set_pilot_flag()
## add_pilot_flag()
##
## pilot_id
## pilot_id_as_string
## set_pilot_id([Integer, ..])
## add_pilot_id(Integer)
##
## path -> aString
## set_path(aString)
##
## dump, dump_header
##
################################################################
##
## set_modified
## modified?
##
## in_day?(aDate)
## in_exception?(aDate)
## in_duration?(aDate)
## in_cond?(aDate)
## in_category?(aString)
##
## occur_max, occur_min,
##
## occur_on?(aDate)
## occur_intermonth?
## occur_multiple?
## occur_any?
##
## error?
## error_message

class MhcScheduleItem

  DURATION_MIN = MhcDate .new(1970,  1,  2)
  DURATION_MAX = MhcDate .new(2037, 12, 31)

  MON_REGEX = MhcDate::M_LABEL .join('|')
  WEK_REGEX = MhcDate::W_LABEL .join('|')
  ORD_REGEX = MhcDate::O_LABEL .join('|')

  MON_LONG_REGEX = MhcDate::M_LONG_LABEL .join('|')
  WEK_LONG_REGEX = MhcDate::W_LONG_LABEL .join('|')

  HDR_REGEX = '(Subject|Location|Day|Time|Category|Cond|Duration|Alarm|Record-Id)'

  ALM_UNITS = {'Minute' => 60, 'Hour' => 60 * 60, 'Day' => 60 * 60 * 24}
  ALM_LABEL = ALM_UNITS .keys
  ALM_REGEX = ALM_LABEL .join('|')

  def initialize(path_or_string = nil, is_path = true)
    clear
    if path_or_string
      if is_path
        init_by_path(path_or_string)
      else
        init_by_string(path_or_string)
      end
    end
    set_rec_id(create_record_id) if ! rec_id
    set_modified(false, 'initialize')
  end

  ################################################################
  ## access methods to each field.

  ## subject
  ##   subject -> aString
  ##   set_subject(aString)
  def subject
    return @subject
  end

  def set_subject(str)
    @subject = str .to_s
    @subject .force_encoding("ASCII-8BIT") if RUBY_VERSION .to_f >= 1.9
    set_modified(true, 'set_subject')
    return self
  end

  ## location
  def location
    return @location
  end

  def set_location(str)
    @location = str .to_s
    set_modified(true, 'set_location')
    return self
  end

  ## day
  def day
    return @day
  end

  def day_as_string
    return (@day .collect{|x| x .to_s} +
            @exception .collect{|x| '!' + x .to_s}) .join(' ')
  end

  def add_day(date)
    ## First, check to see if Duration: encumbers
    raise("Change Duration First\n") if !in_duration?(date)

    ## if date is in exception, remove, it.
    if in_exception?(date)
      @exception .delete(date)
      set_modified(true, 'add_day')
    end

    ## It is happy if Cond: covers the date.
    return self if in_cond?(date)

    if !in_day?(date)
      (@day << date) .uniq!
      set_modified(true, 'add_day')
    end
    return self
  end

  def del_day(date)
    return self if !occur_on?(date)

    @day .delete(date)             # xxx: does it surely delete ymd?
    set_modified(true, 'del_day') #      it may be over estimation
    if in_cond?(date)
      (@exception << date) .uniq!
    end
    return self
  end

  ## exception
  def exception
    return @exception
  end

  ## time
  def time
    return [@time_b, @time_e] if @time_b
    return nil
  end

  def time_as_string
    return @time_b .to_s + (@time_e ? '-' : '') + @time_e .to_s
  end

  def time_b; return @time_b; end
  def time_e; return @time_e; end

  def set_time(b, e = nil)
    @time_b, @time_e = b, e
    set_modified(true, 'set_time')
    return self
  end

  ## alarm
  def alarm
    return @alarm
  end

  def alarm_as_string
    alarm_str = ''

    if @alarm
      alarm_str = "#{@alarm /60} minute"
      if @alarm > 60 * 99 || @alarm % 3600 == 0
        alarm_str = "#{@alarm /3600} hour"
      elsif @alarm > 3600 * 99 || @alarm % 84600 == 0
        alarm_str = "#{@alarm /84600} day"
      end
    end
    return alarm_str
  end

  def set_alarm(sec)
    ## set alarm  time in sec.
    ## nil means no alarm.
    @alarm = sec
    set_modified(true, 'set_alarm')
    return self
  end

  ## category
  def category
    return @category
  end

  def category_as_string
    return @category .join(' ')
  end

  def set_category(str)
    if str .kind_of?(Array)
      @category = str
    else
      @category = []
      str .to_s .split .each{|s|
        s .force_encoding("ASCII-8BIT") if RUBY_VERSION .to_f >= 1.9
        @category << s
      }
    end
    @category .uniq!
    set_modified(true, 'set_category')
    return self
  end

  def add_category(str)
    @category << str .capitalize
    return self
  end

  def del_category(str)
    @category .delete(str .capitalize)
    return self
  end

  ## cond
  def cond
    return @cond_mon + @cond_ord + @cond_wek + @cond_num
  end

  def cond_as_string
    return (@cond_mon + @cond_ord + @cond_wek + @cond_num) .join(' ')
  end

  def cond_mon; return @cond_mon; end
  def cond_ord; return @cond_ord; end
  def cond_wek; return @cond_wek; end
  def cond_num; return @cond_num; end

  def set_cond(str_or_array) ## arg is String or Array of String.
    @cond_mon, @cond_ord, @cond_wek, @cond_num = [], [], [], []
    set_modified(true, 'set_cond')

    if str_or_array .kind_of?(Array)
      array = str_or_array
    else
      array = str_or_array .split
    end

    array .each{|s| add_cond(s)}
    return self
  end

  def add_cond(cond)
    cond = cond .capitalize
    case cond
    when /^(#{MON_REGEX})$/oi
      (@cond_mon << cond) .uniq!
    when /^(#{MON_LONG_REGEX})$/oi
      cond = MhcDate::M_LABEL[ MhcDate::M_LONG_LABEL .index(cond) ]
      (@cond_mon << cond) .uniq!
    when /^(#{ORD_REGEX})$/oi
      (@cond_ord << cond) .uniq!
    when /^(#{WEK_REGEX})$/oi
      (@cond_wek << cond) .uniq!
    when /^(#{WEK_LONG_REGEX})$/oi
      cond = MhcDate::W_LABEL[ MhcDate::W_LONG_LABEL .index(cond) ]
      (@cond_wek << cond) .uniq!
    when /^\d+$/
      (@cond_num << format("%02d", cond .to_i)) .uniq!
    end

    set_modified(true, 'add_cond') # it may be over estimation.
    return self
  end

  def del_cond(cond)
    cond = cond .capitalize
    case cond
    when /^(#{MON_REGEX})$/oi
      @cond_mon .delete(cond)
    when /^(#{ORD_REGEX})$/oi
      @cond_ord .delete(cond)
    when /^(#{WEK_REGEX})$/oi
      @cond_wek .delete(cond)
    when /^\d\d?$/
      @cond_num .delete(format("%02d", cond .to_i))
    end
    set_modified(true, 'del_cond') # it may be over estimation.
    return self
  end

  ## duration
  def duration
    return [@duration_b, @duration_e] if @duration_b || @duration_e
    return nil
  end

  def duration_as_string
    if @duration_b || @duration_e
      return @duration_b .to_s + '-' + @duration_e .to_s
    else
      return ''
    end
  end

  def duration_b; return @duration_b; end
  def duration_e; return @duration_e; end

  def set_duration(b, e)
    @duration_b, @duration_e = b, e
    set_modified(true, 'set_duration')
    return self
  end

  ## record-id
  def rec_id
    return @rec_id
  end

  def set_rec_id(r)
    @rec_id = r .to_s
    return self
  end

  def dump_without_xsc_header
    #koie: if no descripton, dont convert non X-SC headers.
    if description .to_s == ''
      return ''
    end
    hdrs = non_xsc_header .to_s .sub(/\n+\z/n, '')
    hdrs += "\n" if hdrs != ''

    desc = description .to_s
    desc += "\n" if desc != '' and desc !~ /\n\z/n

    return hdrs + (desc != '' ? "\n" : '') + desc
  end

  def dump
    hdrs = non_xsc_header .to_s .sub(/\n+\z/n, '')
    hdrs += "\n" if hdrs != ''

    desc = description .to_s
    desc += "\n" if desc != '' and desc !~ /\n\z/n

    return dump_header + hdrs + "\n" + desc
  end

  ## non_xsc_header
  def non_xsc_header
    return @non_xsc_header
  end

  def set_non_xsc_header(txt)
    @non_xsc_header = txt
    set_modified(true, 'set_description')
    return self
  end

  ## description
  def description
    if @description
      return @description
    elsif @path && File .file?(@path)
      file    = File .open(@path, "r")
      file .gets("\n\n")
      @description = file .gets(nil)
      file .close
    end
    return @description
  end

  def set_description(txt)
    @description = txt
    set_modified(true, 'set_description')
    return self
  end

#    ## description
#    def description(all = false)
#      if @description
#        content = @description

#      elsif @path && File .file?(@path)
#        file    = File .open(@path, "r")
#        content = file .gets(nil)
#        file .close
#      else
#        content = ''
#      end

#      if content =~ /^[a-z-]+:/pi && content !~ /^http:/ip
#        hdr, val = content .split("\n\n", 2)
#        hdr << "\n"
#        hdr .gsub!(/^X-SC-#{HDR_REGEX}:[^\n]+\n([ \t]+[^\n]*\n)*/ino, '')
#      else
#        hdr, val = '', content
#      end

#      hdr << dump_header if all
#      hdr << "\n" if hdr != ''

#      @description = (hdr << val .to_s)
#      return @description
#    end

  ## priority
  def priority
    return @priority
  end

  def priority_as_string
    if @priority == 0
      return ""
    else
      return @priority .to_s
    end
  end

  def set_priority(pri)
    begin
      @priority = pri .to_i
    rescue
      @priority = 0
    ensure
    end
  end

  ## pilot_flag
  def pilot_flag
  end
  def set_pilot_flag
  end
  def add_pilot_flag
  end

  ## pilot_id
  def pilot_id
  end

  def pilot_id_as_string
    @pilot_id ? @pilot_id .join(' ') : ''
  end

  def set_pilot_id(id_array)
    @pilot_id = []
    id_array .each{|id|
      add_pilot_id(id)
    }
    return self
  end

  def add_pilot_id(id)
    @pilot_id = [] if !@pilot_id
    @pilot_id << id
    set_modified(true, 'add_pilot_id')
    return self
  end

  ## path
  def path
    return @path
  end

  def set_path(path)
    @path = path
    set_modified(true, 'set_path')
    return self
  end

  ################################################################
  ## dump for save.

#  def dump
#    description(true)
#  end

  def dump_header
    return "X-SC-Subject: #{subject}\n"        +
      "X-SC-Location: #{location}\n"           +
      "X-SC-Day: #{day_as_string}\n"           +
      "X-SC-Time: #{time_as_string}\n"         +
      "X-SC-Category: #{category_as_string}\n" +
      "X-SC-Priority: #{priority_as_string}\n" +
      "X-SC-Cond: #{cond_as_string}\n"         +
      "X-SC-Duration: #{duration_as_string}\n" +
      "X-SC-Alarm: #{alarm_as_string}\n"       +
      "X-SC-Record-Id: #{rec_id}\n"
    ## "X-SC-Debug-Path: #{path}\n"
  end

  ################################################################
  ## various tests.
  ##
  def set_modified(bool, msg)
    print "#{msg} modified #{self} to #{bool}\n" if $DEBUG
    @modified = bool
  end

  def modified?
    ## impurity check.
    return @modified
  end

  def in_day?(date)
    return (@day .include?(date)) ? true : false
  end

  def in_exception?(date)
    ## Does the date exist as a exception?
    return (@exception .include?(date)) ? true : false
  end

  def in_duration?(date)
    return false if (@duration_b && date < @duration_b)
    return false if (@duration_e && date > @duration_e)
    return true
  end

  def in_cond?(d)
    ## Does the date match to the Cond: field?
    mon, wek, ord, num = [d .m_s, d .w_s, d .o_s, d .d_s]

    return false if !(@cond_mon .empty? || @cond_mon .include?(mon))
    return true  if  @cond_num .include?(num)
    return false if !(@cond_ord .empty? || @cond_ord .include?(ord)) &&
                    !(d .o_last? && @cond_ord .include?('Last'))
    return true  if @cond_wek .include?(wek)
    return false
  end

  def in_category?(category)
    if category .kind_of?(String)
      return @category .include?(category)
    else
      ## assumes an array.
      return !(@category & category) .empty?
    end
  end

  ################################################################
  def occur_max
    if !(@cond_wek .empty? && @cond_num .empty?)
      max = DURATION_MAX .dup
    else
      # Sometimes palm makes empty @day - @exception.
      # ex. X-SC-Day: 20000911 !20000911
      max = (@day - @exception) .max
      max = DURATION_MAX .dup if !max
    end
    max = @duration_e if max && @duration_e && max > @duration_e
    return max
  end

  def occur_min
    if !(@cond_wek .empty? && @cond_num .empty?)
      min = DURATION_MIN .dup
    else
      min = (@day - @exception) .min
      # Sometimes palm makes empty @day - @exception.
      # ex. X-SC-Day: 20000911 !20000911
      min = DURATION_MIN .dup if !min
    end
    min = @duration_b if min && @duration_b  && min < @duration_b
    return min
  end

  def occur_on?(date)
    return (in_day?(date) || in_cond?(date)) &&
           !in_exception?(date) && in_duration?(date)
  end

  def occur_inter_month?
    if occur_min && occur_max
      return true if occur_min .y < occur_max .y
      return true if occur_min .m < occur_max .m
      return false ## means all occurences are in one month.
    else
      return false ## means no occurence exists.
    end
  end

  def todo?
    return /todo/i =~ category_as_string
  end

  def occur_multiple?
    if occur_min && occur_max
      return true if occur_min != occur_max
    else
      return false ## means no occurrence specified.
    end
    return false ## means there is only one occurrence.
  end

  def occur_any?
    ## Does this article have any occurence?
    return !(occur_min .nil?)
  end

  ################################################################
  def error?
    return true  if !(@subject && @subject != '')
    return true  if !occur_any?
    return false
  end

  def error_message
    msg  = []
    msg << 'no subject'     if  !(@subject && @subject != '')
    msg << 'no occurences'  if  !occur_any?
    return msg .join(',')
  end

  ################################################################
  ## convert to palm Datebook record.
  def to_palm
    ret     = []
    day_cp  = day .dup

    ### for repeat
    beg, fin = occur_min, occur_max
    fin = nil if fin == DURATION_MAX

    ## First, treat X-SC-Day: field.
    while day_cp .length > 0
      if day_cp .length > 1 && day_cp .length == day_cp .max - day_cp .min + 1
        ## repeat in a series of days -- make up as a daily.
        ret << mk_palm_skel .set_daily(day_cp .min, day_cp .max, 1)
        day_cp = []
      else
        ret << mk_palm_skel .set_nonrepeat_date(day_cp .shift)
      end
    end

    ## Second, treat X-SC-Cond: field.
    if cond .length == cond_wek .length && cond_wek .length > 0
      ## weekly
      weeks = []
      for w in 0 .. 6
        weeks << cond_wek .include?(MhcDate::W_LABEL[w]) ? true : false
      end
      ret << mk_palm_skel .set_weekly(beg, fin, 1, weeks)

    elsif  cond_ord .length >= 1  &&
          !cond_ord .include?('5th') &&
           cond_wek .length >= 1  &&
           cond_num .length == 0  &&
           cond_mon .length == 0
      ## monthly by day
      cond_ord .each{|ord_str|
        cond_wek .each{|wek_str|
          ord = MhcDate::O_LABEL .index(ord_str)
          wek = MhcDate::W_LABEL .index(wek_str)
          if ord == 5
            ord = 4
          end
          sch2 = MhcScheduleItem .new .add_cond(ord_str) .add_cond(wek_str)
          beg2 = beg .dup
          while !sch2 .occur_on?(beg2) ## xxx 多分これは不要?
            beg2 .succ!
          end
          ret << mk_palm_skel .set_monthly_by_day(beg2, fin, 1, ord, wek)
        }
      }
    elsif cond_num .length == 1 &&
          cond_num .length == cond .length
      ## monthly by date
      while !occur_on?(beg) ## xxx こっちは必要
        beg .succ!
      end
      ret << mk_palm_skel .set_monthly_by_date(beg, fin, 1)

    elsif cond_num .length == 1 &&
          cond_mon .length == 1 &&
          cond_wek .length == 0 &&
          cond_ord .length == 0
      ## yearly by date
      y = beg .y
      m = MhcDate::M_LABEL .index(cond_mon[0]) + 1
      d = cond_num[0] .to_i
      date = MhcDate .new(y, m, d)
      if date < beg
        date .y_succ!
      end
      ## 2/29 はどうする?
      ret << mk_palm_skel .set_yearly(date, fin, 1)

    elsif cond_ord .length == 1  &&
          cond_ord[0] != '5th'   &&
          cond_wek .length == 1  &&
          cond_num .length == 0  &&
          cond_mon .length == 1
      ## yearly by day
      ord = MhcDate::O_LABEL .index(cond_ord[0])
      wek = MhcDate::W_LABEL .index(cond_wek[0])
      m   = MhcDate::M_LABEL .index(cond_mon[0]) + 1
      date = MhcDate .new(beg .y, m, 1)
      if date .m < beg .m
        date .y_succ!
      end
      while !occur_on?(date)
        date .succ!
      end
      ret << mk_palm_skel .set_monthly_by_day(date, fin, 12, ord, wek)

    elsif cond .empty?
      ## do nothing
    else
      ## conversion failed.
      ret = []
    end

    if ret .empty?
      # STDERR .print "#{occur_min .to_js} : #{subject} unsupported. ignored..\n"
      return nil
    else
      return ret
    end
  end

  ################################################################
  private
  ################################################################
  RECORD_ID_INFO = ['AAAA', nil, 0]

  def create_record_id(domain = 'from.mhc-schedule.rb')
    last_id_rand, last_id_time, last_id_counter = RECORD_ID_INFO

    id_time = Time .now .strftime("%Y%m%d%H%M%S")
    id_user = Process .uid .to_s

    if last_id_time && id_time == last_id_time
      last_id_counter += 1
      last_id_rand .succ!
      id_rand = last_id_rand
    else
      last_id_rand = 'AAAA'
      id_rand = last_id_rand
      last_id_counter = 0
    end

    id_rand += '-' + $$ .to_s
    last_id_time = id_time

    RECORD_ID_INFO[0], RECORD_ID_INFO[1], RECORD_ID_INFO[2] =
      last_id_rand, last_id_time, last_id_counter
    return '<' + id_time + id_rand + '.' + id_user + '@' + domain + '>'
  end

  def mk_palm_skel
    pi_rec = PilotApptRecord .new
    pi_rec .set_alarm(alarm)
    pi_rec .set_time(time_b, time_e)

    exception .each{|date|
      pi_rec .add_exception(date)
    }

    datebk3_icon = nil
    category .each{|cat|
      datebk3_icon = cat if cat =~ /^\#\#@@@.@@@$/
    }
    contents = dump_without_xsc_header
    contents = '' if contents =~ /\A\s+\z/n   ## \s includes \n
    contents = datebk3_icon + "\n" + contents if datebk3_icon
    #koie: if contents is empty, dont set note.
    if contents != ""
    pi_rec .set_note(contents)
    end #koie

    if (location .to_s != '')
      pi_rec .set_description(subject + '[' + location .to_s + ']')
    else
      pi_rec .set_description(subject)
    end

    return pi_rec
  end

  def clear
    @cond_mon, @cond_ord, @cond_wek, @cond_num = [], [], [], []
    @day, @exception, @category, @pilot_id     = [], [], [], []

    @subject, @location, @description, @path   = '', nil, nil, nil
    @time_b, @time_e                           = nil, nil
    @duration_b, @duration_e                   = nil, nil
    @alarm, @rec_id                            = nil, nil

    @modified                                  = false
    @non_xsc_header                            = ''
    return self
  end

  def init_by_path(path)
    ## 1. set instance variables corresponding to X-SC-*: headers.
    ## 2. set @non_xsc_header as one string by non X-SC-*: headers.
    ## 3. set @description

    clear
    file = File .open(path, "r")
    all_headers =  file .gets("\n\n")
    file .close
    @non_xsc_header, xsc_header_hash = select_headers(all_headers)
    parse_xsc_headers(xsc_header_hash)
    @path = path
    # @description will be loaded on demand from the file.
    return self
  end

  def init_by_string(string)
    clear
    all_headers, @description  = string .split(/\n\n/, 2)
    @description = nil if @description == ''
    @non_xsc_header, xsc_header_hash = select_headers(all_headers)
    parse_xsc_headers(xsc_header_hash)
    return self
  end

  def parse_xsc_headers(hash)
    hash .each_pair{|key,val|
      case key
      when 'day:'
        while (val != '')
          case val
          when /^!/
            is_exception = true
          when /^\d+/
            if is_exception
              @exception << MhcDate .new($&)
              is_exception = false
            else
              @day << MhcDate .new($&)
            end
          when /^[^!\d]+/
            # discard the word.
          else
            # never occured.
          end
          val = $'
        end

      when 'date:' ## backward compatibility
        if (val =~ /(\d+)\s+([A-Z][a-z][a-z])\s+(\d+)\s+(\d\d:\d\d)/)
          dd, mm, yy, hhmm = $1 .to_i, $2, $3 .to_i  + 1900 , $4
          mm = ("JanFebMarAprMayJunJulAugSepOctNovDec" .index(mm)) / 3 + 1
          @time_b = (hhmm == '00:00') ? nil : MhcTime .new(hhmm)
          @day << MhcDate .new(yy, mm, dd)
        end

      when 'subject:'
        @subject = val

      when 'location:'
        @location = val

      when 'time:'
        @time_b, @time_e = val .split('-')
        @time_b = MhcTime .new(@time_b) if @time_b
        @time_e = MhcTime .new(@time_e) if @time_e

      when 'duration:'
        b, e = val .split('-')
        @duration_b = (b .nil? || b == '') ? nil : MhcDate .new(b)
        @duration_e = (e .nil? || e == '') ? nil : MhcDate .new(e)

      when 'category:'
        val .split .each{|c| @category << c .capitalize}

      when 'cond:'
        val .split .each{|d|
          case d
          when /^(#{MON_REGEX})$/oi
            @cond_mon << d .capitalize
          when /^(#{MON_LONG_REGEX})$/oi
            d = MhcDate::M_LABEL[ MhcDate::M_LONG_LABEL .index(d) ]
            @cond_mon << d .capitalize
          when /^(#{ORD_REGEX})$/oi
            @cond_ord << d .capitalize
          when /^(#{WEK_REGEX})$/oi
            @cond_wek << d .capitalize
          when /^(#{WEK_LONG_REGEX})$/oi
            d = MhcDate::W_LABEL[ MhcDate::W_LONG_LABEL. index(d) ]
            @cond_wek << d .capitalize
          when /^\d+$/
            @cond_num << format("%02d", d .to_i)
          end
        }

      when 'alarm:'
        if val =~ /^(\d+)\s*(#{ALM_REGEX})$/i
          @alarm = ($1 .to_i) * ALM_UNITS[$2 .capitalize]
        end

      when 'record-id:'
        @rec_id = val

      when 'priority:'
        if val =~ /^(\d+)\s*$/i
          begin
            @priority = $1 .to_i
          rescue
            @priority = 0
          end
        else
          @priority = 0
        end

      end ## case ##
    }
    return self
  end

#    def header_to_hash(header)
#      hdr = {}
#      if header
#        header .gsub(/\n\s+/, ' ') .split("\n") .each{|line|
#       if (line =~ /^X-SC-([^:]+:)(.*)/ni)
#         key, val = $1 .downcase, $2 .strip
#         hdr[key] = val if (val != '')
#       end
#        }
#      end
#      return hdr
#    end

  ## return:  X-SC-*:  as a hash and
  ##       :  non X-SC-*: as one string.
  def select_headers(header)
    xsc, non_xsc, xsc_key = {}, '', nil
    if header
#      header .gsub(/\n\s+/, ' ') .split("\n") .each{|line|
      header .split("\n") .each{|line|
        if line =~ /^\S/
          key, val = line .split(':', 2)
          if (key =~ /^X-SC-(.*)/ni)
            xsc_key = $1 .downcase + ':'
            xsc[xsc_key] = (val != '') ? val .strip : ''
          else
            xsc_key = nil
            non_xsc += line + "\n"
          end
        elsif line =~ /^\s/
          if xsc_key
            xsc[xsc_key] += ' ' + line
          else
            non_xsc += line + "\n"
          end
        end
      }
    end
    return non_xsc, xsc
  end
end

################################################################
class File
  MTIME_FILE = ".mhc-mtime"

  def File.utime2(atime, mtime, obj)
    if File .directory?(obj)
      if File .file?(obj + '/' + MTIME_FILE)
        File .utime(atime, mtime, obj + '/' + MTIME_FILE)
      else
        f = File .open(obj + '/' + MTIME_FILE, "w")
        f .print 'x' # FreeBSD requires this.
        f .fsync if f .respond_to?("fsync")
        f .close
      end
    end
    File .utime(atime, mtime, obj)
  end

  def File.mtime2(obj)
    if (File .directory?(obj)) and (File .file?(obj + '/' + MTIME_FILE))
      File .mtime(obj + '/' + MTIME_FILE)
    else
      File .mtime(obj)
    end
  end
end

class MhcScheduleDB

  HOME        = ENV['HOME'] || ''
  DEF_BASEDIR = HOME + '/Mail/schedule'
  DEF_RCFILE  = HOME + '/.schedule'

  ALL = 'all'

  def initialize(basedir = DEF_BASEDIR, *rcfiles)
    @db        = {}
    @mtime     = {}
    @basedir   = basedir
    @rcfiles   = rcfiles .length == 0 ? [DEF_RCFILE] : rcfiles
    @slots     = @rcfiles + [@basedir + '/intersect']
    @alarm     = nil
    @log       = MhcLog .new(@basedir + '/.mhc-db-log')
  end

  def signal_connect(sig, &p)
    if !@alarm
      @alarm = Alarm .new
      @alarm .signal_connect('sec-changed'){
        if update_all
          print "MhcScheduleDB: emit updated signal\n" if $DEBUG
          @alarm .signal_emit('updated')
        end
      }
    end
    return @alarm .signal_connect(sig, &p)
  end

  def signal_disconnect(id)
    @alarm .signal_disconnect(id)
  end

  def del_sch(sch, add_log = true)
    if (old_path = sch .path)
      old_slot = File .dirname(old_path)
      trash_path = get_new_path(@basedir + '/trash')
      File .rename(old_path, trash_path)
      now = Time .now
      File .utime2(now, now, old_slot)
      print "mv #{old_path} -> #{trash_path}\n" if $DEBUG
    end
    if add_log
      @log .add_entry(MhcLogEntry .new('D', Time .now,
                                       sch .rec_id, sch .path, sch .subject))
    end
    sch .set_path(nil)
    return self
  end

  def add_sch(sch, add_log = true)
    new_slot = sch_to_slot(sch)
    old_slot = File .dirname(sch .path) if sch .path

    begin
      now = Time .now
      old_path = sch .path

      if old_slot && new_slot == old_slot
        new_path = old_path
      else
        new_path = get_new_path(new_slot)
      end

      contents = sch .dump
      f = File .open(new_path, "w")
      f << contents
      f .fsync if f .respond_to?("fsync")
      f .close
      print "#{old_path} -> #{new_path}\n" if $DEBUG

      File .utime2(now, now, new_slot)
      sch .set_path(new_path)

      trash_path = get_new_path(@basedir + '/trash')
      if old_path && File .exists?(old_path) && old_path != new_path
        File .rename(old_path, trash_path)
        File .utime2(now, now, old_slot)
        print "#{old_path} -> #{trash_path}\n" if $DEBUG
      end
    rescue
      raise("#{$!}\nWrite/Move #{old_path} -> #{new_path} failed.")
    end
    if add_log
      @log .add_entry(MhcLogEntry .new('M', Time .now,
                                       sch .rec_id, sch .path, sch .subject))
    end
    sch .set_modified(false, 'add_sch')
    return self
  end

  def each_sch(from = nil, to = nil)
    if !from || !to
      now  = MhcDate .new
      from = now .m_succ(-3)
      to   = now .m_succ( 4)
    end
    hash = {}
    search(from, to) .each{|d, sch_ary|
      sch_ary .each{|sch|
        if !hash[sch]
          yield(sch)
          hash[sch] = true
        end
      }
    }
  end

  def search(from, to, category = nil, do_update = true)
    ret = []
    for date in from .. to
      ret << [date, search1(date, category, do_update)]
    end
    return ret
  end

  def m_search(date, category = nil)
    update(date)
    return search(date .m_first_day, date .m_last_day, category, false)
  end

  def holiday?(date)
    !search1(date, 'Holiday') .empty?
  end

  def search1(d, category = nil, do_update = true)
    mon, wek, ord, day, date = d .m_s, d .w_s, d .o_s, d .d_s, d
    last = 'Last'
    ret = []
    category_ary, category_is_invert = nil, false

    if category
      if category =~ /!/
        category_is_invert = true
        category = category .delete('!')
      else
        category_is_invert = false
      end
      category_ary = category .split .collect{|x| x .capitalize}
    end

    search_key = [date, mon+ord+wek, mon+ALL+wek, ALL+ord+wek,
                  ALL+ALL+wek, mon+day, ALL+day, mon+ALL, ALL+ALL]
    search_key << mon+last+wek << ALL+last+wek if d .o_last?

    update(d) if do_update
    to_slots(d) .each{|slot|
      search_key .each{|key|
        if @db[slot][key] .is_a?(Array)
          @db[slot][key] .each{|item|
            if (item .in_duration?(date)) && !(item .in_exception?(date)) &&
                (!category ||
                 (!category_is_invert &&  item .in_category?(category_ary)) ||
                 ( category_is_invert && !item .in_category?(category_ary)))
              ret << item
            end
          }
        end
      }
    }
    return ret .sort{|a,b| a .time_b .to_s <=> b .time_b .to_s} .uniq
  end

  ################
  private
  ################
  def regist(slot, o)
    day, mon, ord, wek, num =
      o.day, o.cond_mon, o.cond_ord, o.cond_wek, o.cond_num

    day .each{|ymd|
      _regist(slot, ymd, o)
    }
    mon = [ALL] if (mon .empty?)
    ord = [ALL] if (ord .empty?)

    mon .each{|mon|
      ord .each{|ord|
        wek .each{|wek|
          _regist(slot, mon+ord+wek, o)
        }
      }
      num .each{|num|
        _regist(slot, mon + format("%02d", num .to_i), o)
      }
      if (num.empty? && wek.empty? && (day.empty? || mon != ALL))
        _regist(slot, mon + ALL, o)
      end
    }
  end

  def makedir_or_higher(dir)
    return true if File .directory?(dir)
    parent = File .dirname(dir)
    if makedir_or_higher(parent)
      Dir .mkdir(dir)
      File .open(dir, "r") {|f| f .sync} if File .method_defined?("fsync")
      return true
    end
    return false
  end

  def get_new_path(slot)
    return nil if !makedir_or_higher(slot)
    new = 1
    Dir .open(slot) .each{|file|
      if (file =~ /^\d+$/)
        num = file .to_i
        new = num + 1 if new <= num
      end
    }
    return slot + '/' + new .to_s
  end

  def to_slots(date)
    return @slots + [@basedir + '/' + format("%04d/%02d", date .y, date .m)]
  end

  def all_slots
    return @slots + Dir .glob(@basedir + '/[0-9]*/[0-9]*') .sort
  end

  def update(date)
    ret = false
    to_slots(date) .each{|slot|
      ret |= update_slot(slot)
    }
    return ret
  end

  def update_all
    ret = false
    @db .each_key{|slot|
      ret |= update_slot(slot)
    }
    return ret
  end

  def update_slot(slot)
    @db[slot] = {} if @db[slot] .nil?

    return false if !modified?(slot)
#    STDERR .print "scanning '#{slot}'\n"

    clear_slot(slot)

    if (File .file?(slot))
      ## read as a rcfile.
      file = File .open(slot, "r")

#        while(header = file .gets("\n\n"))
#       regist(slot, MhcScheduleItem .new(header, false))
#        end

      header = ''
      while (line = file .gets)
        line .force_encoding("ASCII-8BIT") if RUBY_VERSION .to_f >= 1.9
        next if line =~ /^#/
        if line == "\n"
          if  header != ''
            regist(slot, MhcScheduleItem .new(header, false))
            header = ''
          end
        else
          header += line
        end
      end
      file .close
      if  header != ''
        regist(slot, MhcScheduleItem .new(header, false))
      end

    elsif (File .directory?(slot))
      ## read as a yyyy/mm folder.
      Dir .open(slot) .each{|file|
        if (file =~ /^\d+$/)
          path = slot + '/' + file
          regist(slot, sch = MhcScheduleItem .new(path))
        end
      }
    end
    return true
  end

  def sch_to_slot(sch)
    if sch .occur_inter_month? or sch .todo?
      return @basedir + '/intersect'
    else
      date = sch .occur_min
      return @basedir + '/' + format("%04d/%02d", date .y, date .m)
    end
  end

  def clear_slot(slot)
    @db[slot] = {}
  end

  def _regist(slot, key, obj)
    # print "_regist #{key .inspect}\n"
    @db[slot]      = {} if @db[slot] .nil?
    @db[slot][key] = [] if @db[slot][key] .nil?
    @db[slot][key] << obj
  end

  def modified?(slot)
    return false if !File .exists?(slot)

    if @mtime[slot] .nil?
      @mtime[slot] = File .mtime2(slot)
      return true
    end

    if @mtime[slot] < (t = File .mtime2(slot))
      @mtime[slot] = t
      return true
    else
      return false
    end
  end
end

################################################################
# Log maintenance functions.
#
# M 2000-04-25 00:06:08 <20.nom@.nomcom> ~nom/Mail/schedule/2000/04/1 Luncheon
# D 2000-04-25 00:06:08 <20.nom@.nomcom> ~nom/Mail/schedule/2000/04/1 Luncheon
# S 2000-04-25 00:06:08 user_id
#
class MhcLog

  def initialize(filename)
    @filename = filename
  end

  def add_entry(entry)
    file = File .open(@filename, "a+")
    file .print "#{entry}\n"
    file .fsync if file .respond_to?("fsync")
    file .close
  end

  def each_entry
    begin
      file = File .open(@filename)
      while line = file .gets
        yield(MhcLogEntry .new(line .chomp))
      end
      file .close
    rescue
    end
  end

  def entries()
    arry = []
    each_entry{|e|
      arry << e
    }
    return arry
  end

  def shrink_entries(user_id)
    hash = {}
    each_entry{|e|
      if e .status == 'S' and e .rec_id == user_id
        hash .clear
      else
        hash[e .rec_id] = e
      end
    }
    return hash .values
  end
end

################
class MhcLogEntry
  attr :status
  attr :mtime
  attr :rec_id
  attr :path
  attr :subject

  def initialize(status, mtime = nil, rec_id = nil, path = nil, subject = nil)
    if mtime .nil?
      init_from_string(status)
    else
      @status, @mtime, @rec_id, @path, @subject =
        status, mtime, rec_id, path, subject
    end
  end

  def to_s
    return [
      @status,
      @mtime .strftime("%Y-%m-%d %H:%M:%S"),
      @rec_id,
      @path,
      @subject
    ] .join(' ')
  end

  ################
  private
  ################
  def init_from_string(line)
    str = line .chomp
    status, yymmdd, hhmmss, rec_id, path, subject = str .split
    yy, mm, dd = yymmdd .split('-')
    h,  m,  s  = hhmmss .split(':')

    mtime = Time .local(yy .to_i, mm .to_i, dd .to_i,
                        h  .to_i, m  .to_i, s  .to_i)
    @status, @mtime, @rec_id, @path, @subject =
      status, mtime, rec_id, path, subject
  end
end

## MHC Alarm クラス
##
## MHC Alarm クラスは、MhcScheduleDB から、先の予定をスキャンしてきて、
## Alarm を発行してほしい時間順にソートした配列を保存しておく。予定時
## 間が来たら、signal を発行する。
##
## 何者かによって、DB が変更されたら、保存している Alarm 情報が無効に
## なってしまうので、再スキャンしする。
##
## 1. make_alarm_table : @alarm_table へ予定表の保存
##
##    今日の日付から、LOOK_AHEAD_DAYS 日分先の予定を scan, sort する
##
##    aTime はアラーム発行時間 (予定の時間ではない)
##    xTime を予定の時間だとすると、
##
##    now   <= aTime <= xTime  な予定 --> @alarm_table に保存
##    aTime <= now   <= xTime  な予定 --> 即 signal を発行
##    aTime <= xTime <= now    な予定 --> 捨てる
##
##    @alarm_table =  [[aTime, aMhcScheduleItem], ... ]
##
##
## 2. スケジュールが何者かによって変更されてしまったとき
##
##    DB から updated signal を拾って、1 を実行する。そのとき、
##
##    > aTime <= now   <= xTime  な予定 --> 即 signal を発行
##
##    は実行しない。
##
## 3. 1分ごとに、
##
##    a. @alarm_table の先頭と現在時刻 now を比較
##    b. aTime <= now なら signal を emit。@alarm_table から捨てて a. に戻る
##
## 4. 1日毎に、
##
##    日が変わる毎に、最後に scan した日付の 1日先の内容を
##    1. の方法で @alarm_table に追加。
##

class MhcAlarm
  LOOK_AHEAD_DAYS   = 100

  def initialize(db = MhcScheduleDB .new)
    @db          = db
    @date_begin  = nil
    @sig_conduit = Alarm .new
    @alarm_table = []

    @sig_conduit .signal_connect('min-changed'){
      print "MhcAlarm: tick\n"  if $DEBUG
      check_alarm_table
    }

    @sig_conduit .signal_connect('day-changed'){
      print "MhcAlarm: day_changed\n" if $DEBUG
      update_alarm_table
    }

    @db .signal_connect('updated'){
      @alarm_table = []
      make_alarm_table(MhcDate .new, LOOK_AHEAD_DAYS, false)
      check_alarm_table
    }
    make_alarm_table(MhcDate .new, LOOK_AHEAD_DAYS, true)
  end

  def signal_connect(sig, &p)
    @sig_conduit .signal_connect(sig, &p)
  end

  def check
    check_alarm_table
  end

  ## for debug
  def dump_alarm_table
    print "DUMP #{@alarm_table .length}\n" if $DEBUG
    @alarm_table .each{|x|
      atime, sch = x
      print "#{atime} #{sch .subject}\n"
    }
  end
  ################################################################
  private

  ## invoked when initialize and rescan.
  def make_alarm_table(date_begin, ahead_days, is_initialize = false)
    @date_begin  = date_begin
    now          = Time .now

    for i in 1 .. ahead_days
      @db .search1(@date_begin) .each{|sch|
        if (sch .alarm)
          xtime = @date_begin .to_t(sch .time_b || MhcTime .new(0, 0))
          atime = xtime - sch .alarm

          if now <= atime
            @alarm_table << [atime, sch]
          elsif now <= xtime && is_initialize
            @alarm_table << [atime, sch]
          end
        end
      }
      @date_begin = @date_begin .succ
    end
    @alarm_table .sort!{|a, b| a[0] <=> b[0]} if @alarm_table
    if $DEBUG
      print "MhcAlarm::make_alarm_table\n"
      dump_alarm_table
    end
  end

  ## invoked once a minute
  def check_alarm_table
    now = Time .now

    while @alarm_table[0] && @alarm_table[0][0] <= now
      atime, sch = @alarm_table .shift
      xdate = time_to_date(atime + sch .alarm)

      printf("MhcAlarm: check_alarm_table emit !! %-8s     %s\n",
             atime .to_s, sch .subject)  if $DEBUG

      @sig_conduit .signal_emit('time-arrived', xdate , sch)
    end
  end

  ## invoked once a day
  def update_alarm_table
    shortage = MhcDate .new - @date_begin + LOOK_AHEAD_DAYS
    if $DEBUG
      print "MhcAlarm: update_alarm_table in? #{@date_begin} + #{shortage}\n"
    end

    if shortage > 0
      if $DEBUG
        print "MhcAlarm: update_alarm_table in #{@date_begin} + #{shortage}\n"
      end
      make_alarm_table(@date_begin, shortage)
    end
    dump_alarm_table if $DEBUG
  end

  def time_to_date(time)
    return MhcDate .new(*time .to_a .indexes(5, 4, 3))
  end

end

if false
  $alarm = MhcAlarm .new
  $alarm .signal_connect('time-arrived'){|date, sch|
    print "signal : #{date .to_js}   #{sch .subject}\n"
  }
  sleep
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

### mhc-schedule.rb ends here
