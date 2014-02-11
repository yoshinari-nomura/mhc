# -*- coding: utf-8 -*-
### mhc-palm.rb
##
## Author:  Yoshinari Nomura <nom@quickhack.net>
##
## Created: 1999/09/01
## Revised: $Date: 2009/01/07 00:15:05 $
##

require 'mhc-date'
begin
  require 'mhc_pilib'
rescue LoadError
#  STDERR .print "Warning: require 'mhc_pilib' was failed."
end

################################################################
##
## fundamental class 1/4
##
## Pilot    -- connection management.
##
class Pilot
  attr :sd

  def initialize(port = '/dev/pilot')
    @sd = PiLib .openSock(port)
  end

  def listen
    @sd = PiLib .listenSock(@sd)
    return nil if @sd .nil?
    return self
  end

  def close
    PiLib .closeSock(@sd)
    return self
  end

  ## Add an entry into the HotSync log on the Pilot.
  ## \n is OK, as usual. You may invoke this command once or more before
  ## calling EndOfSync (sockClose), but it is not required.
  def add_synclog(string)
    PiLib .dlp_AddSyncLogEntry(@sd, string)
    return self
  end

  ## reset lastSyncPC in the UserInfo to 0
  def reset_lastsync_pc
    PiLib .dlp_ResetLastSyncPC(@sd)
    return self
  end

  def get_time
    return PiLib .dlp_GetSysDateTime(@sd)
  end

  def set_time(time)
    PiLib .dlp_SetSysDateTime(@sd, time)
    return self
  end
end

################################################################
##
## fundamental classe 2/4
##
## PilotDB
##
class PilotDB
  def initialize(pi, dbname)
    @sd = pi .sd
    @db = PiLib .dlp_OpenDB(@sd, dbname)
    @recClass = PilotRecord
  end

  def close
    PiLib .dlp_CloseDB(@sd, @db)
  end

  def record_by_index(i)
    ary = PiLib .dlp_ReadRecordByIndex(@sd, @db, i)
    return nil if ary .nil?
    return @recClass .new(*ary)
  end

  def record_by_id(id)
    ary = PiLib .dlp_ReadRecordById(@sd, @db, id)
    return nil if ary .nil?
    return @recClass .new(*ary)
  end

  def each_record
    i = 0
    while (rec = record_by_index(i)) != nil
      yield rec
      i += 1
    end
  end

  def write_record(rec)

    rec_array = rec .to_a
    new_id = PiLib .dlp_WriteRecord(@sd, @db, rec_array)
    return new_id
  end

  def delete_by_id(id)
    PiLib .dlp_DeleteRecord(@sd, @db, false, id)
    return self
  end

  def delete_all
    PiLib .dlp_DeleteRecord(@sd, @db, true, 0)
    return self
  end

  ## Deletes all records in the opened database which are marked as archived
  ## or deleted.
  def cleanup_record
    PiLib .dlp_CleanUpDatabase(@sd, @db)
    return self
  end

  ## For record databases, reset all dirty flags. For both record and
  ## resource databases, set the last sync time to now.
  def reset_sync_flags
    PiLib .dlp_ResetSyncFlags(@sd, @db)
    return self
  end

  def get_app_info()
    return PiLib .dlp_ReadAppBlock(@sd, @db)
  end
end

################################################################
##
## fundamental class 3/4
##
## PilotRecord -- PilotDB record
##
class PilotRecord
  def initialize(id = 0, attr = 0, category = 0, data = '')
    @id, @attr, @category, @data = id, attr, category, data
    unpack
    check
  end

  attr :id
  def set_id(id)
    raise "Integer required." if !(id .is_a?(Integer))
    @id = id
    return self
  end

  def attribute_deleted?       ; return  (@attr & 0x80 != 0) ;end
  def attribute_dirty?         ; return  (@attr & 0x40 != 0) ;end
  def attribute_busy?          ; return  (@attr & 0x20 != 0) ;end
  def attribute_secret?        ; return  (@attr & 0x10 != 0) ;end
  def attribute_archived?      ; return  (@attr & 0x08 != 0) ;end

  def set_attribute_deleted    ; @attr |= 0x80; return self  ;end
  def set_attribute_dirty      ; @attr |= 0x40; return self  ;end
  def set_attribute_busy       ; @attr |= 0x20; return self  ;end
  def set_attribute_secret     ; @attr |= 0x10; return self  ;end
  def set_attribute_archived   ; @attr |= 0x08; return self  ;end

  def reset_attribute_deleted  ; @attr &= ~0x80; return self ;end
  def reset_attribute_dirty    ; @attr &= ~0x40; return self ;end
  def reset_attribute_busy     ; @attr &= ~0x20; return self ;end
  def reset_attribute_secret   ; @attr &= ~0x10; return self ;end
  def reset_attribute_archived ; @attr &= ~0x08; return self ;end

  def set_attribute(attr)
    raise "Integer required." if !(attr .is_a?(Integer))
    @attr = attr
    return self
  end

  def attribute
    return @attr
  end

  def attribute_string
    attr_str = []
    attr_str << 'Deleted'  if (@attr & 0x80 != 0)
    attr_str << 'Dirty'    if (@attr & 0x40 != 0)
    attr_str << 'Busy'     if (@attr & 0x20 != 0)
    attr_str << 'Secret'   if (@attr & 0x10 != 0)
    attr_str << 'Archived' if (@attr & 0x08 != 0)
    return attr_str .join(' ')
  end

  attr :category
  def set_category(category)
    raise "Integer required." if !(category .is_a?(Integer))
    @category = category
    return self
  end

  def data
    pack
    return @data
  end

  def set_data(data)
    raise "String required." if !(data .is_a?(String))
    @data = data
    return self
  end

  def to_a
    check
    pack
    return [@id, @attr, @category, @data]
  end

  private
  def pack;    end
  def unpack;  end

  def check
    raise "Id must be Integer."         if !(@id .is_a?(Integer))
    raise "Attribute must be Integer."  if !(@attr .is_a?(Integer))
    raise "Category must be Integer."   if !(@category .is_a?(Integer))
    raise "Data must be String."        if !(@data .is_a?(String))
    return self
  end
end


################################################################
##
## fundamental class 4/4
##
## PilotFile  -- File DB
##
class PilotFile
  ## these methods are defined in pilib.c
  ## get_app_info
  ## read_record
  ## close

  def initialize(filename, recClass = PilotRecord)
    @recClass = recClass
  end

  def each_record
    i = 0
    while (rec = record_by_index(i)) != nil
      yield rec
      i += 1
    end
  end

  def record_by_index(i)
    ary = read_record(i)
    return nil if ary .nil?
    return @recClass .new(*ary)
  end
end

################################################################
##
## classes for Memo
##
class PilotMemoDB < PilotDB
  def initialize(pi, dbname)
    super
    @recClass = PilotMemoRecord
  end
end

class PilotMemoRecord < PilotRecord
  undef set_data

  def set_memo_data(string)
    @memo_data = string
    return self
  end

  def memo_data
    return @memo_data
  end

  def unpack
    @memo_data = @data .sub(/\0$/, '')
    return self
  end

  def pack
    @data = Kconv::tosjis(@memo_data || '') + "\0"
    return self
  end
end

################################################################
##
## classes for Datebook
##

class PilotApptDB < PilotDB
  def initialize(pi, dbname)
    super
    @recClass = PilotApptRecord
  end
end


class PilotApptRecord < PilotRecord
  UNIT_TYPE   = %w(minute hour day)
  ORDER_TYPE  = %w(1st 2nd 3rd 4th Last)
  WEEK_TYPE   = %w(Sun Mon Tue Wed Thu Fri Sat)
  MONTH_TYPE  = %w(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)
  REPEAT_TYPE = %w(None Daily Weekly MonthlyByDay MonthlyByDate Yearly)

  undef set_data

  def repeat?    ; return @repeatType != 0                ;end
  def event?     ; return @event                          ;end
  def alarm?     ; return @alarm                          ;end
  def category?  ; return @category == 0 ? false : true   ;end
  def forever?   ; return @repeatForever                  ;end

  def repeatType ; return REPEAT_TYPE[@repeatType]        ;end
  def repeatDay
    return "#{ORDER_TYPE[@repeatDay / 7]} #{WEEK_TYPE[@repeatDay % 7]}"
  end

  def alarm
    if alarm?
      return @advance .to_s  + ' ' + UNIT_TYPE[@advanceUnits]
    else
      return ''
    end
  end

  def repeatDays
    ary  = []
    for i in (0 .. 6)
      ary << WEEK_TYPE[i] if @repeatDays[i]
    end
    return ary .join(' ')
  end

  ## b  @event                  時間指定がないイベントかどうか
  ## t  @beg                    開始日付、時間。
  ##                            (repeat の場合は、duration の開始でもある)
  ## t  @fin                    終わりの時間 (date 部分は beg と同じにする)
  ##                            event == 1 のときは、time 部分は全部 0
  ## b  @alarm                  1 or 0
  ## i  @advance                0-99
  ## i  @advanceUnits           units = ['minute', 'hour', 'day'];

  ## i     @repeatType                  None,Daily Weekly MonthlyByDay,
  ##                            MonthlyByDate,Yearly
  ##                            byday -> cond (@repeatDay を信用)
  ##                            bydate -> num @beg .day を信用。
  ## b     @repeatForever               repeatEnd を信用していいかどうか。
  ##                            repeatEnd は信用してはならない。
  ## t     @repeatEnd           Duration end (date 部分だけ)
  ## i     @repeatFrequency             int
  ## i     @repeatDay           o = repeatDay /7, w = repeatDay % 7
  ##                            5th がないのはなぜ?
  ## b[7]  @repeatDays          Sun, Mon, Tue, 1 or 0
  ## i     @repeatWeekstart             いつも 0

  ## i     @exceptions,         0?
  ## t[x]  @exception           []
  ## s     @description         NULL  or Subject:
  ## s     @note                NULL  or 本文


  ################################################################
  ## スケジュールタイプによらない
  ##

  ## set alarm in second.
  def set_alarm(alarm)
    raise "Type error: requires Integer\n" if alarm && !alarm .is_a?(Integer)

    if alarm
      @alarm = true

      if alarm % 86400 == 0 && alarm <= 86400 * 99
        @alarmUnit = 2 ## day
        @advance   = alarm / 86400
      elsif alarm % 3600 == 0 && alarm <= 3600 * 99
        @alarmUnit = 1 ## hour
        @advance   = alarm / 3600
      elsif alarm % 60 == 0 && alarm <= 60 * 99
        @alarmUnit = 0 ## minute
        @advance   = alarm / 60
      else
        raise "Could not convert alarm."
      end
    else
      @alarm        = false
      @advance      = 0
      @advanceUnits = 0
    end
  end

  def set_time(b = nil, e = nil)
    raise "Type error: requires MhcTime\n" if b && !b .is_a?(MhcTime)
    raise "Type error: requires MhcTime\n" if e && !e .is_a?(MhcTime)
    if b
      @event = false
      e = b if !e ## 終了時間を指定していなかったら、開始と同じに
      @beg = replace_time(@beg, b .hh, b .mm)
      @fin = replace_time(@fin, e .hh, e .mm)
    else
      @event = true
      @beg = replace_time(@beg, 0, 0)
      @fin = replace_time(@fin, 0, 0)
    end
  end

  def add_exception(date)
    ## repeatType = None のときは、exception を設定しても意味がない?
    raise "Type error: requires MhcDate\n" if !date .is_a?(MhcDate)
    @exception << date .to_t
    @exceptions = @exception .length
    return self
  end

  def set_note(txt)
    raise "Type error: requires String\n" if !txt .is_a?(String)
    @note = Kconv::tosjis(txt)
    return self
  end

  def set_description(txt)  ## subject
    raise "Type error: requires String\n" if !txt .is_a?(String)
    @description = Kconv::tosjis(txt)
    return self
  end

  ################################################################
  ## スケジュールタイプ別
  ##

  ## 普通の リピートしないやつ
  ##
  def set_nonrepeat_date(date)
    raise "Type error: requires MhcDate\n" if !date .is_a?(MhcDate)
    @repeatType = 0
    @beg = replace_date(@beg, date .y, date .m, date .d)
    @fin = replace_date(@fin, date .y, date .m, date .d)
    return self
  end

  ## repeatType -> Daily
  ##
  ## x-sc-day を sort して、 1日間隔に並んでいる &&
  ## x-sc-cond は空
  ##
  ## n 日毎
  ## duration_b - duration_e まで
  def set_daily(beg, fin, freq)
    set_duration(beg, fin) ## beg, fin の型チェックもする
    set_frequency(freq)    ## freq     の型チェックもする
    ## xxx: 先に全部チェックしてからでないと rollback できない。。
    @repeatType = 1
    return self
  end

  ## repeatType -> Weekly
  ##
  ## x-sc-day は空 &&
  ## x-sc-cond は wek だけ(複数可)
  ##
  ## Sun Mon Tue
  ## duration_b - duration_e まで
  ##
  ## weeks = [false, true, true, false, false, false, false]
  ##     -> Mon, Tue
  ##
  def set_weekly(beg, fin, freq, weeks)
    set_duration(beg, fin) ## beg, fin の型チェックもする
    set_frequency(freq)    ## freq     の型チェックもする
    w = []

    if weeks .is_a?(Array) && weeks .length == 7
      weeks .each{|bool|
        if !(bool == true || bool == false)
          raise "Type error: weeks must be bool[7]"
        end
        w << bool
      }
    else
      raise "Type error: weeks must be bool[7]"
    end

    @repeatType      = 2
    @repeatDays      = w
    @repeatWeekstart = 0
    return self
  end

  ## repatType -> Monthly
  ##
  ## x-sc-day は空 &&
  ## ((x-sc-cond は ord が 1個 && ord != 5th && wek が一個だけ) ||
  ##  x-sc-cond は num が 1個だけ)
  ##
  ## 1st Sun  -- by day
  ## 01       -- by date
  ## duration_b - duration_e まで
  ##
  ## ord  .. 0 - 4 の整数 1st, 2nd, 3rd, 4th, Last に対応
  ## wek  .. 0 - 6 の整数 Sun, ... ,Sat            に対応
  ##
  def set_monthly_by_day(beg, fin, freq, ord, wek)
    set_duration(beg, fin) ## beg, fin の型チェックもする
    set_frequency(freq)    ## freq     の型チェックもする

    ## beg の日付が ord, week を満たしているかのチェックが必要
    msg = "Type/Range error: (0< ord <4, 0< wek <6) required. (#{ord}, #{wek})"
    raise msg if !(ord .is_a?(Integer) && 0 <= ord && ord <= 4)
    raise msg if !(wek .is_a?(Integer) && 0 <= wek && wek <= 6)
    raise msg if !((ord == 4 && beg .o_last?) || (ord < 4  && beg .o == ord))

    @repeatDay  = ord * 7 + wek
    @repeatType = 3
    return self
  end

  def set_monthly_by_date(beg, fin, freq)
    set_duration(beg, fin) ## beg, fin の型チェックもする
    set_frequency(freq)    ## freq     の型チェックもする

    ## beg の 日付の部分の 「日」 がそのまま使われる
    @repeatType = 4
    return self
  end

  ## repeatType -> yearly
  ##
  ## x-sc-day は空 &&
  ## x-sc-cond は mon が 1個と num が1個だけ
  ##
  ## Jan 01
  ## duration_b - duration_e まで
  def set_yearly(beg, fin, freq)
    set_duration(beg, fin) ## beg, fin の型チェックもする
    set_frequency(freq)    ## freq     の型チェックもする

    ## beg の 日付の部分の 「月・日」 がそのまま使われる
    @repeatType = 5
    return self
  end

  ################################################################
  ## mhc が使う MhcScheduleItem への変換

  $last_mid_rand = 'AAAA'
  $last_mid_time = nil
  $last_mid_counter = 0

  def create_message_id(domain = 'from.your.palm')
    mid_time = Time .now .strftime("%Y%m%d%H%M%S")
    mid_user = Process .uid .to_s

    if $last_mid_time && mid_time == $last_mid_time
      $last_mid_counter += 1
      $last_mid_rand .succ!
      mid_rand = $last_mid_rand
    else
      $last_mid_rand = 'AAAA'
      mid_rand = $last_mid_rand
      $last_mid_counter = 0
    end

    mid_rand += '-' + $$ .to_s
    $last_mid_time = mid_time
    return '<' + mid_time + mid_rand + '.' + mid_user + '@' + domain + '>'
  end

  def to_xsc
    xsc = {};
    xsc["Record-Id"]  = create_message_id(@id .to_s) # xxx
    xsc["Pilot-Attr"] = attribute_string
    xsc["Pilot-Id"]   = @id
    xsc["Subject"]    = Kconv::tojis(Kconv::toeuc(@description) .sub(/\[[^\]]*\]\s*$/, ''))
    xsc["Location"]   = Kconv::tojis($1) if Kconv::toeuc(@description) =~ /\[([^\]]+)\]\s*$/
    xsc["Note"]       = Kconv::tojis(@note)
    xsc["Category"]   = @category  if category?
    xsc["Alarm"]      = alarm
    xsc["Day"]        = @exception .collect{|t| '!' + t .to_xscday} .join(' ')
    xsc["Day"]       += ' ' + @beg .to_xscday if !repeat?
    if !event?
      if @beg .to_xsctime == @fin .to_xsctime
        xsc["Time"]  = ' ' + @beg .to_xsctime
      else
        xsc["Time"]  = ' ' + @beg .to_xsctime + '-' + @fin .to_xsctime
      end
    end

    if repeat?
      if @repeatFrequency > 1
        STDERR .print "#{@beg} : #{Kconv::tojis(@description)} "
        STDERR .print "unsupported. ignored..\n"
        return nil
      end

      if !forever?
        b, e = @beg .to_xscday, @repeatEnd .to_xscday
        xsc["Duration"] = b + '-' + e
        b_date, e_date = MhcDate .new(b), MhcDate .new(e)
      end

      case repeatType
      when 'Daily'
        if !forever? && (e_date - b_date < 7)
          for d in b_date .. e_date
            xsc["Day"] += ' ' + d .to_s
          end
        else
          xsc["Cond"] = 'Sun Mon Tue Wed Thu Fri Sat' # xxx
        end
      when 'Weekly'
        xsc["Cond"] = repeatDays
      when 'MonthlyByDay'
        xsc["Cond"] = repeatDay
      when 'MonthlyByDate'
        xsc["Cond"] = @beg .day .to_s
      when 'Yearly'
        xsc["Cond"] = MONTH_TYPE[@beg .mon - 1] + ' ' + @beg .day .to_s
      end
    end

    str = ''
    note = ''
    xsc .each{|key, val|
      if key == 'Note'
        note = val
      else
        str += "X-SC-#{key}: #{val}\n"
      end
    }
    x = MhcScheduleItem .new(str, false)

    note_hdr, note_desc, datebk3_icon = conv_note(note)
    x .set_non_xsc_header(note_hdr)
    x .set_description(note_desc)
    x .add_category(datebk3_icon) if datebk3_icon

    x .set_pilot_id([@id])
    return x
  end

  ################################################################
  ## private

  ## Palm のノート -> mhc の body と X-SC-* 以外のヘッダ部分に変換
  def conv_note(string)
    if string =~ /\A(\#\#@@@.@@@)\n([^\z]*)\z/n
      datebk3_icon, string = $1, $2
    end

    part1_is_header = true

    part1, part2 = string .split("\n\n", 2)

    if !(part1 =~ /^[ \t]+/ or part1 =~ /^[A-Za-z0-9_-]+:/)
      part1_is_header = false
    end

    part1 .to_s .split("\n") .each{|line|
      if !(line =~ /^[ \t]+/ or line =~ /^[A-Za-z0-9_-]+:/)
        part1_is_header = false
      end
    }

    if part1_is_header
      header, body = part1, part2
    else
      header, body = nil, string
    end

    return header, body, datebk3_icon
  end

  ## Time クラスインスタンスの 時間部分だけを置き換える
  def replace_time(time, hour, min)
    return Time .local(*time .to_a .indexes(5, 4, 3) + [hour, min])
  end

  ## Time クラスインスタンスの 日付部分だけを置き換える
  def replace_date(time, y, m, d)
    return Time .local(y, m, d, *time .to_a .indexes(2, 1))
  end

  ##
  def set_frequency(freq)
    raise "Type error: freq must be Integer\n"  if !freq .is_a?(Integer)
    @repeatFrequency = freq
  end

  ## 繰り返しの duration 部分を設定する e == nil は forever
  def set_duration(beg, fin)
    raise "Type error: begin must be MhcDate\n"  if !beg .is_a?(MhcDate)
    raise "Type error: end   must be MhcDate\n"  if fin && !fin .is_a?(MhcDate)

    @beg = replace_date(@beg, beg .y, beg .m, beg .d)
    ## @fin の日付部分は、常に @beg と同じになる
    ## duration end は @repeatEnd で設定
    @fin = replace_date(@fin, beg .y, beg .m, beg .d)

    if fin
      @repeatForever = false
      @repeatEnd = Time .local(fin .y, fin .m, fin .d)
    else
      @repeatForever = true
      @repeatEnd = Time .local(1970, 1, 2)
    end
  end

  def unpack
    if @data != ''
      @event, @beg, @fin, @alarm, @advance, @advanceUnits,
        @repeatType, @repeatForever, @repeatEnd, @repeatFrequency,
        @repeatDay,  @repeatDays, @repeatWeekstart, @exceptions,
        @exception,  @description, @note = PiLib .unpack_Appointment(@data)

    else
      @event           = true
      @beg             = Time .local(1970, 1, 2)
      @fin             = Time .local(1970, 1, 2)
      @alarm           = false
      @advance         = 0
      @advanceUnits    = 0
      @repeatType      = 0
      @repeatForever   = false
      @repeatEnd       = Time .local(1970, 1, 2)
      @repeatFrequency = 0
      @repeatDay       = 0
      @repeatDays      = [false] * 7
      @repeatWeekstart = 0
      @exceptions      = 0
      @exception       = []
      @description     = 'No Subject'
      @note            = ''
    end
    return self
  end

  def pack
    ary = [@event, @beg, @fin, @alarm, @advance,
      @advanceUnits, @repeatType, @repeatForever,
      @repeatEnd, @repeatFrequency,
      @repeatDay,  @repeatDays, @repeatWeekstart,
      @exceptions, @exception,  @description, @note]
    @data = PiLib .pack_Appointment(ary)
    return self
  end

end

class Time
  def to_xscday
    return format("%04d%02d%02d", year, mon, day)
 end
  def to_xsctime
    return format("%02d:%02d", hour, min)
  end
end

################################################################
##
## classes for AddressBook
##
class PilotAddressDB < PilotDB
  def initialize(pi, dbname)
    super ## set @id, @attr, @category, @data

    app_info = self .get_app_info

    @catRenamed, @catName, @catID, @catLastUniqueID,
      # [22]       [22]          [8]
      @labels, @labelsRenamed, @phoneLabels,
      @country, @sortByCompany = *PiLib .unpack_AddressAppInfo(app_info)

    ## @labels[22], @labelsRenamed[22]
    ##

    ## @phoneLabels[0..7] =
    ##      [会社, 自宅, Fax, その他, E-mail, 代表, ポケベル, 携帯]

    @recClass = PilotAddRecord
  end
end

class PilotAddressRecord < PilotRecord


  def to_s(labels, ren, phonelabels)
    ret = ''
    # 00-03   labels[i]  に対応
    # 04-11   phoneLabels[@phoneLabel[i - 4]] に対応
    # 12-16   labels[i]  に対応
    # 17-20   labels[i]  に対応 (カスタム)
    # 21      labels[i]  に対応

    for i in (0 .. 2)
      ret += "#{labels[i]}: #{@entry[i]}\n"
    end
    for i in (3 .. 7)
      ret += "#{phonelabels[@phoneLabel[i-3]]}: #{@entry[i]}\n"
    end
    for i in (8 .. 18)
      ret += "#{labels[i]}: #{@entry[i]}\n"
    end

    return ret
  end

  private
  def unpack
    # i[5]         i         s[19]
    @phoneLabel, @showPhone, @entry = *PiLib .unpack_Address(@data)
  end

  def pack
    @data = PiLib .pack_Address([@phoneLabel, @showPhone, @entry])
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

### mhc-palm.rb ends here
