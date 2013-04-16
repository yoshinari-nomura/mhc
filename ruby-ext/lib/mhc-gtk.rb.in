# -*- coding: utf-8 -*-
# -*- ruby -*-

### mhc-gtk.rb
##
## Author:  Yoshinari Nomura <nom@quickhack.net>
##
## Created: 1999/07/16
## Revised: $Date: 2007/02/19 03:01:16 $
##

#$DEBUG = true

require 'gtk2'

require 'mhc-kconv'
require 'mhc-date'
require 'mhc-signal'

module MhcKconv
  def todisp(string)
    Kconv::kconv(string, Kconv::UTF8, Kconv::AUTO)
  end
  module_function :todisp
end

Gtk.init
# xxx: from ruby-gtk 0.23, Gtk::CAN_* changed to Gtk::Widget::CAN_*
#
CAN_DEFAULT = Gtk::Widget::CAN_DEFAULT || Gtk::CAN_DEFAULT
CAN_FOCUS   = Gtk::Widget::CAN_FOCUS   || Gtk::CAN_FOCUS


################################################################
##################  Gtk Setup  #################################
################################################################

XPM_PATH = '@@MHC_XPM_PATH@@'

TIPS = Gtk::Tooltips .new
# TIPS .set_delay(500)

if RUBY_PLATFORM =~ /cygwin/
  ## for windows
  FONTSET = "-unknown-ms ui gothic-normal-r-normal-*-*-100-*-*-p-*-windows-shiftjis"
  FONTSET2 = "-unknown-ms ui gothic-bold-r-normal-*-*-100-*-*-p-*-windows-shiftjis"
  FONT  = Gdk::Font .font_load(FONTSET)
  # FONT2 = Gdk::Font .font_load(FONTSET2)
  Gtk::RC::parse_string <<EOS
style "default"
{
  font = "#{FONTSET}"
}
widget_class "*" style "default"
EOS
else
  FONTSET = "-*-fixed-medium-r-normal--14-*,-*-fixed-medium-r-normal--14-*"
  FONTSET2 = "-*-fixed-bold-r-normal--14-*,-*-fixed-*-r-normal--14-*"
  FONT  = Pango::FontDescription.new("serif normal 10")
  FONT2 = Pango::FontDescription.new("sans normal 10")
  FONT2.set_weight(Pango::FontDescription::WEIGHT_BOLD)
  Gtk::RC::parse_string <<EOS
style "default"
{
  fontset = "#{FONTSET}"
}
widget_class "*" style "default"
EOS
end

STYLE_SATURDAY = Gtk::Style .new .set_font_desc(FONT)
STYLE_HOLIDAY  = Gtk::Style .new .set_font_desc(FONT)
STYLE_TODAY    = Gtk::Style .new .set_font_desc(FONT)
STYLE_WEEKDAY  = Gtk::Style .new .set_font_desc(FONT)

[Gtk::STATE_ACTIVE, Gtk::STATE_INSENSITIVE, Gtk::STATE_NORMAL,
  Gtk::STATE_PRELIGHT, Gtk::STATE_SELECTED] .each {|s|

  STYLE_TODAY    .set_bg(s, 65535, 0, 65535)
  STYLE_SATURDAY .set_fg(s, 0, 0, 65535)
  STYLE_HOLIDAY  .set_fg(s, 65535, 0, 0)
}

STYLE_HASH = {
  'saturday' => STYLE_SATURDAY,
  'holiday'  => STYLE_HOLIDAY,
  'today'    => STYLE_TODAY,
  'weekday'  => STYLE_WEEKDAY,
}

STYLE_ARRAY = [
  STYLE_HOLIDAY, STYLE_WEEKDAY, STYLE_WEEKDAY,
  STYLE_WEEKDAY, STYLE_WEEKDAY, STYLE_WEEKDAY, STYLE_SATURDAY,
]

################################################################
################# Common GUI classes ###########################
################################################################

################################################################
## input alarm
class GtkAlarmEntry < Gtk::HBox
  UNIT_LABEL  = ['minute', 'hour', 'day']
  UNIT_INSEC  = {'minute' => 60, 'hour' => 60 * 60, 'day' => 60 * 60 * 24}
  UNIT_REGEX  = UNIT_LABEL .join('|')

  def initialize(&p)
    super(false, 0)
    @hbx  = Gtk::HBox .new(false, 0)
    @item = nil
    @unit = 'minute'
    @btn = Gtk::CheckButton .new('No Alarm')
    menu  = get_omenu(&p) ## setup @item and @unit and @omenu
    @num  = GtkNumericSpin .new(0, 99, 10, 1, 10)
    lbl   = Gtk::Label .new('Alarm:')
    @hbx .pack_start(lbl,  false, false, 0)
    @hbx .pack_start(@num,  false, false, 0)
    @hbx .pack_start(menu, false, false, 0)

    @num .signal_connect('changed'){
      p .call(dump)
    }

    @btn .signal_connect('toggled'){
      if @btn .active?
        have_paticular_value(false)
        p .call(nil)
      else
        have_paticular_value(true)
        p .call(dump)
      end
    }
    pack_start(@hbx, false, false, 0)
    pack_start(@btn, false, false, 0)
  end

  def set_alarm(sec)
    if !sec
      have_paticular_value(false)
      return self
    end

    have_paticular_value(true)
    UNIT_LABEL .reverse .each{|unit|
      in_sec =  UNIT_INSEC[unit]
      if sec > in_sec
        @num        .set_value(sec / in_sec)
        @item[unit] .set_active(true)
        return self
      end
    }
    @num .set_value(0)
    @item['minute'] .set_active(true)
    return self
  end

  def dump
    if @btn .active?
      return nil
    else
      return (@num .value_as_int) * UNIT_INSEC[@unit]
    end
  end

  private
  def have_paticular_value(bool)
    @hbx .set_sensitive(bool)
    @btn .set_active(!bool)
  end

  def get_omenu(&p)
    @item = {}
    menu  = Gtk::Menu .new
    @omenu = Gtk::OptionMenu .new
    group = nil
    UNIT_LABEL .each{|unit|
      item = Gtk::RadioMenuItem::new(group, unit)
      item .signal_connect('activate', unit){|w, unit|
        @unit = unit
        @omenu .set_history(UNIT_LABEL .index(unit))
        p .call(dump) if p
      }
      group = item .group
      menu .append(item)
      item .set_active(true) if unit == 'minute'
      item .show
      @item[unit] = item
    }
    @omenu .set_menu(menu)
    @omenu .set_history(0)
    return @omenu
  end
end



################################################################
## Input Time hh:mm

class GtkTimeEdit < Gtk::Table
  FILL = Gtk::FILL

  def initialize(time, &p)
    hh, mm = time .to_a
    super(2, 2, false)
    @min_time = MhcTime .new(23, 59)

    @h = GtkNumericCombo .new(0, 23, hh, 1, 5)
    @m = GtkNumericCombo .new(0, 59, mm / 5 * 5, 10, 30)

    @h .entry .signal_connect('changed'){p .call}
    @m .entry .signal_connect('changed'){p .call}

    h_lbl = Gtk::Label .new('Hour:') .set_alignment(0, 0.5)
    m_lbl = Gtk::Label .new('Min:')  .set_alignment(0, 0.5)
    attach(h_lbl, 0, 1, 0, 1, FILL, FILL, 0, 0)
    attach(m_lbl, 1, 2, 0, 1, FILL, FILL, 0, 0)
    attach(@h,    0, 1, 1, 2, FILL, FILL, 0, 0)
    attach(@m,    1, 2, 1, 2, FILL, FILL, 0, 0)
  end

  def set_value(t)
    @h .set_number(t .hh)
    @m .set_number(t .mm)
    return self
  end

  def get_value
    h, m = @h .dump, @m .dump
    return MhcTime .new(h, m)
  end
  alias dump get_value

  def set_min(time)
    @min_time = time
    @h .set_min(time .hh)
    if @h .dump == @min_time .hh
      @m .set_min(@min_time .mm)
    end
    return self
  end

end

################################################################
## TimeRange

class GtkTimeRangeEdit < Gtk::VBox

  def initialize(b, e, &p)
    super(false, 0)

    @hbx = Gtk::HBox .new(false, 0)
    @b   = GtkTimeEdit .new(b){
      hh, mm = @b .dump .to_a
      @e .set_min(MhcTime .new(hh + 1, 0))
      # @e .set_min(@b .dump)

      p .call(@b .dump, @e .dump)
    }
    @e   = GtkTimeEdit .new(e){p .call(@b .dump, @e .dump)}
    @btn = Gtk::CheckButton .new('No Particular Time')
    lbl  = Gtk::Label .new(' -- ') .set_alignment(0, 0.75)

    @hbx .pack_start(@b , false, false, 0)
    @hbx .pack_start(lbl, false, false, 0)
    @hbx .pack_start(@e , false, false, 0)

    pack_start(@hbx, false, false, 0)
    pack_start(@btn, false, false, 0)

    @btn .signal_connect('toggled'){
      if @btn .active?
        have_paticular_value(false)
        p .call(nil, nil)
      else
        have_paticular_value(true)
        @hbx .set_sensitive(true)
        p .call(@b .dump, @e .dump)
      end
    }
  end

  def set_value(b, e)
    if b
      have_paticular_value(true)
      @b .set_value(b)
      if e
        @e .set_value(e)
      else
        @e .set_value(b)
      end
    else
      have_paticular_value(false)
    end
    return self
  end

  def have_paticular_value(bool)
    @hbx .set_sensitive(bool)
    @btn .set_active(!bool)
  end

  def dump
    if @btn .active?
      return 'None'
    else
      return [@b .get_value, @e .get_value]
    end
  end
end

################################################################
## Input Date

class GtkDateEdit < Gtk::Table
  #FILL = Gtk::FILL | Gtk::EXPAND | Gtk::SHRINK
  FILL = Gtk::FILL

  def initialize(date, &p)
    super(3, 3, false)
    yyyy, mm, dd = date .to_a

    @y = GtkNumericSpin  .new(1970, yyyy + 30, yyyy, 1, 5)
    @m = GtkNumericCombo .new(1, 12, mm, 1, 5)
    @d = GtkNumericSpin  .new(1, 31, dd, 1, 5)

    [@y, @m, @d] .each{|box|
      box .entry .signal_connect('changed'){
        p .call
      }
    }
    y_lbl = Gtk::Label .new('Year:') .set_alignment(0, 0.5)
    m_lbl = Gtk::Label .new('Mon:')  .set_alignment(0, 0.5)
    d_lbl = Gtk::Label .new('Day:')  .set_alignment(0, 0.5)

    self .attach(y_lbl, 0, 1, 0, 1, FILL, FILL, 0, 0)
    self .attach(m_lbl, 1, 2, 0, 1, FILL, FILL, 0, 0)
    self .attach(d_lbl, 2, 3, 0, 1, FILL, FILL, 0, 0)

    self .attach(@y, 0, 1, 1, 2, FILL, FILL, 0, 0)
    self .attach(@m, 1, 2, 1, 2, FILL, FILL, 0, 0)
    self .attach(@d, 2, 3, 1, 2, FILL, FILL, 0, 0)
  end

  def set_value(d)
    @y .set_number(d .y)
    @m .set_number(d .m)
    @d .set_number(d .d)
    return self
  end

  def btn
    return @b
  end

  def get_value
    return MhcDate .new(@y .dump, @m .dump, @d .dump)
  end
  alias dump get_value
end

################################################################
## Date Range

class GtkDateRangeEdit < Gtk::VBox
  def initialize(date1, date2, &p)
    super(false, 0)

    @hbx = Gtk::HBox .new(false, 0)
    @b   = GtkDateEdit .new(date1){p .call(@b .dump, @e .dump)}
    @e   = GtkDateEdit .new(date2){p .call(@b .dump, @e .dump)}
    @btn = Gtk::CheckButton .new('No Particular Duration')
    lbl  = Gtk::Label .new('  --  ') .set_alignment(0, 0.75)

    @hbx .pack_start(@b , false, false, 0)
    @hbx .pack_start(lbl, false, false, 0)
    @hbx .pack_start(@e , false, false, 0)

    pack_start(@hbx, false, false, 0)
    pack_start(@btn, false, false, 0)

    @btn .signal_connect('toggled'){
      if @btn .active?
        have_paticular_value(false)
        #@hbx .set_sensitive(false)
        p .call(nil, nil)
      else
        have_paticular_value(true)
        #@hbx .set_sensitive(true)
        p .call(@b .dump, @e .dump)
      end
    }
    #have_paticular_value(false)
  end

  def have_paticular_value(bool)
    @hbx .set_sensitive(bool)
    @btn .set_active(!bool)
  end

  def set_value(b, e)
    if b
      have_paticular_value(true)
      @b .set_value(b)
      if e
        @e .set_value(e)
      else
        @e .set_value(b)
      end
    else
      have_paticular_value(false)
    end
    return self
  end

#   def get_value
#     return [@b .get_value, @e .get_value]
#   end
#   alias dump get_value
end


################################################################
## Japanese Himekuri Calendar (What do you call it in English ?)

class GtkDayBook < Gtk::VBox

  def initialize(date = MhcDate .new, buttons = nil, need_clist = true)
    super(false, 0)
    set_border_width(0)

    @date      = date
    @tip       = ''

    @hbx = GtkButtonBar .new(buttons) if buttons
    @btn = Gtk::Button .new('') .set_border_width(0) \
           .unset_flags(CAN_FOCUS) .set_relief(Gtk::RELIEF_NONE)

    if need_clist
      @lst = Gtk::ListStore.new(String, String)
      @lsv = Gtk::TreeView.new(@lst).set_headers_visible(false) \
                .set_rules_hint(true)
      @lsv.append_column(Gtk::TreeViewColumn.new("Time",
                                                 Gtk::CellRendererText.new,
                                                 :text => 0) \
                         .set_max_width(42))
      @lsv.append_column(Gtk::TreeViewColumn.new("Desc",
                                                 Gtk::CellRendererText.new,
                                                 :text => 1) \
                         .set_max_width(56))
    end
    pack_start(@btn, false, false, 0)
    pack_start(@lsv, true,  true,  0) if need_clist
    pack_start(@hbx, false, true,  0) if buttons
    set_date(@date)
  end

  def append(item, time = '')
    if @lst
      iter = @lst.append
      iter[0] = time
      iter[1] = MhcKconv::todisp(item)
    end
    return self
  end

  def set_tip(tip)
    # @tip = tip ? MhcKconv::todisp(tip .to_s).tr("\007", " ") : nil
    @tip = tip ? MhcKconv::todisp(tip .to_s) : nil
    TIPS .set_tip(@btn, @tip, nil)
    return self
  end

  def append_tip(tip)
    @tip = (@tip .to_s) + tip # + tip.tr("\007", " ")
    set_tip(MhcKconv::todisp(@tip))
    return self
  end

  def set_date(date, title_short = false)
    @date = date
    @btn .set_relief(Gtk::RELIEF_NONE)
    @btn .set_sensitive(true)
    @lst .clear  if @lst
    set_tip(nil)

    set_style(STYLE_ARRAY[@date .w])
    set_style('today') if @date .today?
    @btn .child .set_text(title_short ? @date .d .to_s : @date .to_js)
    return self
  end

  def date
    return @date
  end

  def set_style(c)
    if c .is_a?(String)
      case c
      when 'today'
        @btn .set_relief(Gtk::RELIEF_NORMAL)
        return self
      when 'busy'
        c = @btn .child .style .dup .set_font_desc(FONT2)
      else
        c = STYLE_HASH[c]
      end
    end
    @btn .set_style(c)
    @btn .child .set_style(c)
    return self
  end

  def get_text(r, c)
    @lst .get_text(r, c) if @lst
  end

  def signal_connect(sig, &p)
    case sig
    when 'day-btn-clicked'
      @btn .signal_connect('clicked', p)
    when 'day-lst-clicked'
      if @lst
        @lsv .signal_connect('row-activated'){|w,path,t|
          p .call(self, path.indices[0].to_i)
        }
      end
    when /-btn-clicked$/
      @hbx .signal_connect(sig, &p)
    else
      super
    end
  end
end

################################################################
## Original Calendar Widget

class GtkCalendar < Gtk::VBox

  FILL = Gtk::FILL | Gtk::SHRINK | Gtk::EXPAND
  NONE = Gtk::SHRINK
  if ENV['LANG'] =~ /^ja/i
    WEEK_L = ['日', '月', '火', '水', '木', '金', '土']
    WEEK_S = ['日', '月', '火', '水', '木', '金', '土']
  else
    WEEK_L = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
    WEEK_S = ['Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa']
  end

  def initialize(date, buttons = nil,
                 need_title = false, is_small = false, need_clist = true)
    super(false, 0)

    week_label = is_small ? WEEK_S : WEEK_L
    @SignalConduit = SignalConduit .new
    @dList  = []
    @date   = date

    @hbx   = GtkButtonBar .new(buttons) if buttons
    @m_tbl = Gtk::Table .new(6, 7, true)
    w_tbl  = Gtk::Table .new(1, 7, true)
    @title  = Gtk::Label .new(@date .ym_js) if need_title

    ## add week label
    for w in 0 .. 6
      wlabel = Gtk::Label .new(MhcKconv::todisp(week_label[w]))
      wlabel .set_style(STYLE_ARRAY[w])
      w_tbl .attach(wlabel, w, w + 1, 0, 1, FILL, NONE, 5, 0)
    end

    (0 .. 41) .each{|i|
      x = i % 7
      y = i / 7
      @dList[i] = GtkDayBook .new(date, nil, need_clist)
      @m_tbl .attach(@dList[i], x, x + 1, y, y + 1, FILL, FILL, 0, 0)

      @dList[i] .signal_connect('day-btn-clicked'){
        @SignalConduit .signal_emit('day-btn-clicked', pos_to_date(i))
      }
      @dList[i] .signal_connect('day-lst-clicked'){|w, r|
        @SignalConduit .signal_emit('day-lst-clicked', w, pos_to_d(i), r)
      }
    }
    pack_start(@title, false, false, 0) if need_title
    pack_start(@hbx,   false, false, 0) if buttons
    pack_start(w_tbl,  false, false, 0)
    pack_start(@m_tbl, true,  true,  0)

    set_date(@date)
    signal_connect('show'){hide_garbage}
  end

  def set_date(date)
    @date   = date .m_first_day
    @offset = @date .w - 1

    for i in 0 .. 41
      if on?(i)
        dd = pos_to_date(i)
        @dList[i] .set_date(dd, true) \
                .set_tip(MhcKconv::todisp("#{dd .d}日")) .show
      else
        @dList[i] .hide
      end
    end
    @title .set_text(@date .ym_js) if @title
    return self
  end

  def next_month
    @date .m_succ!
    set_date(@date)
  end

  def prev_month
    @date .m_succ!(-1)
    set_date(@date)
  end

  def this_month
    @date = MhcDate .new
    set_date(@date)
  end

  def date;  return @date;  end
  def d(dd); return @dList[dd + @offset]; end

  def signal_connect(sig, &p)
    case sig
    when 'day-lst-clicked'
      @SignalConduit .signal_connect('day-lst-clicked', &p)
    when 'day-btn-clicked'
      @SignalConduit .signal_connect('day-btn-clicked', &p)
    when /-btn-clicked$/
      @hbx .signal_connect(sig, &p)
    else
      super
    end
  end

  private
  def hide_garbage
    for i in 0 .. 41
      @dList[i] .hide if !on?(i)
    end
    return self
  end

  def pos_to_date(p)
    return MhcDate .new(@date .y, @date .m, p - @offset)
  end

  def on?(p)
    d = p - @offset
    return (d > 0 and d <= @date .m_days)
  end

  def pos_to_d(p)
    return p - @offset
  end
end

################################################################
## confirm
class GtkConfirm < Gtk::Dialog
  def initialize(msg, btns = 1, &p)
    b = [[Gtk::Stock::OK, Gtk::Dialog::RESPONSE_ACCEPT]]
    if btns > 1
      b << [Gtk::Stock::CANCEL, Gtk::Dialog::RESPONSE_REJECT]
    end
    super(msg, nil, Gtk::Dialog::MODAL, *b)
    self.vbox.pack_start(Gtk::Label.new(msg)).show_all
    run do |response|
      case response
      when Gtk::Dialog::RESPONSE_ACCEPT
        p.call(true) if p
      when Gtk::Dialog::RESPONSE_REJECT
        p.call(false) if p
      end
    end
    destroy
  end
end

################################################################
## Watch toplevel windows.

class GtkToplevel < Gtk::Window
  def initialize
    super(Gtk::Window::TOPLEVEL)
    signal_connect('destroy'){
      print "GtkToplevel destroyed\n" if $DEBUG
      Gtk.main_quit  if active_other_windows == 0
    }
  end

  def destroyed?
    return self .inspect  =~ / destroyed/
  end

  def active_other_windows
    c = 0
    ObjectSpace::each_object(GtkToplevel){|obj|
      c += 1 if (!obj .destroyed? && obj != self && obj .visible?)
    }
    return c
  end

#  def show
#    pop_position
#    super
#  end

  def show_all
    pop_position if !visible?
    super
  end
  alias show show_all

  def hide
    print "GtkToplevel hide\n" if $DEBUG
    Gtk.main_quit  if active_other_windows == 0
    push_position
    super
  end

  def hide_all
    print "GtkToplevel hide_all\n" if $DEBUG
    Gtk.main_quit  if active_other_windows == 0
    push_position
    super
  end

  def push_position
    @pos = self .position
    print "push_position #{@pos .inspect}\n" if $DEBUG
  end

  def pop_position
    if @pos
      print "pop_position #{@pos[0]}, #{@pos[1]}\n" if @pos && $DEBUG
      self .move(@pos[0], @pos[1])
    end
  end

  def confirm
    top = Gtk::Window .new(Gtk::WINDOW_POPUP)

    vbx = Gtk::VBox .new(false, 0)
    hbx = Gtk::HBox .new(true , 0)

    y = Gtk::Button .new('OK')
    y .flags |= CAN_DEFAULT
    y .signal_connect('clicked'){Gtk.main_quit}
    hbx .pack_start(y, true, true, 0)

    n = Gtk::Button .new('Cancel')
    n .flags |= CAN_DEFAULT
    n .grab_default
    n .signal_connect('clicked'){top .destroy}
    hbx .pack_start(n, true, true, 0)

    vbx .pack_start(Gtk::Label .new('Exit GtkCalendar?'))
    vbx .pack_start(hbx, false, false, 0)

    top .add(vbx) .set_modal(true).set_size_request(250,100)#set_usize(250,100)
    top .set_position(Gtk::WIN_POS_CENTER) .show_all
  end
end

################################################################
## Simple file viewer

class GtkFileViewer < Gtk::VBox
#   RED     = Gdk::Color .new(0xffff, 0x0000, 0x0000)
#   GREEN   = Gdk::Color .new(0x0000, 0xffff, 0x0000)
#   BLUE    = Gdk::Color .new(0x0000, 0x0000, 0xffff)
#   BLACK   = Gdk::Color .new(0x0000, 0x0000, 0x0000)
#   PURPLE  = Gdk::Color .new(0xffff, 0x0000, 0xffff)
#   WHITE   = Gdk::Color .new(0xffff, 0xffff, 0xffff)
#   ORANGE  = Gdk::Color .new(56360, 24247, 6553)

  def initialize(text_editable = false)
    super(false, 0)
    @modified = false
    @text_editable = text_editable

    hbx  = Gtk::HBox .new(false, 0)

    ## @txt = Gtk::Text .new(nil, vad)
    @txt = Gtk::TextView .new(nil)                            ##

    @txt .set_editable(@text_editable)

    ## vad  = Gtk::Adjustment .new(0, 0, 0, 0, 0, 0)
    ## @vsc  = Gtk::VScrollbar .new(vad)
    @vsc = Gtk::ScrolledWindow .new(nil, nil)                  ##
    @vsc .set_policy(Gtk::POLICY_NEVER, Gtk::POLICY_AUTOMATIC) ##
    @vsc .add(@txt)                                            ##

    @height = 14 ## (@txt .get_style .font .string_width '0') * 2 ## xxxx
    # @txt .signal_connect('key_press_event'){|w, ev| less_key(w, ev .string)}
    @txt .buffer .signal_connect('changed'){|w| @modified = true}

    ## hbx .pack_start(@txt, true, true,   0)
    ## hbx .pack_start(@vsc,  false, false, 0)
    hbx .pack_start(@vsc,  true, true, 0)                      ##

    pack_start(hbx,  true, true, 0)
    #open(path)
  end


  def less_key(w, key)
    value     = @vsc .vadjustment .value
    lower     = @vsc .vadjustment .lower
    upper     = @vsc .vadjustment .upper
    page_size = @vsc .vadjustment .page_size
    value_min = lower
    value_max = upper - page_size
    case key
    when 'j'
      value += @height
    when 'k'
      value -= @height
    when ' '
      value += page_size
    when 'b', "\x08"
      value -= page_size
    when '<'
      value = value_min
    when '>'
      value = value_max
    end

    if value > value_max
      value = value_max
    elsif value < value_min
      value = value_min
    end
    @vsc .vadjustment .value= value
  end

#   def open(path)
#     if path .kind_of?(String) and File .exist?(path)
#       text = Kconv::toeuc(File .open(path) .read)
#       replace_text(text)
#     else
#       replace_text("\n\n")
#     end
#     set_modified(false, 'open')
#   end

  def replace_text(text)
    @txt .set_editable(true) # .freeze
    @txt .buffer .set_text("")
#     hdr, value = text .split("\n\n", 2)
#      hdr .to_s .each_line{|line|
#        case line
#        when /^Subject:/           ;color = BLUE
#        when /^X-SC-Subject:/      ;color = BLUE
#        when /^From:/              ;color = PURPLE
#        #when /^To:|Cc:/           ;color = ORANGE
#        #when /^Date:/             ;color = GREEN
#        when /^X-SC-(Time|Day):/   ;color = RED
#        else                       ;color = BLACK
#        end
#        @txt .insert(nil, color, nil, line) if line != ''
#      }
#     @txt .insert(nil, RED,   WHITE, hdr .to_s)
#     @txt .insert(nil, BLACK, nil, "\n\n" + value .to_s)

    @txt .buffer .insert(@txt.buffer.start_iter, MhcKconv::todisp(text))
    @txt .set_editable(@text_editable) #.thaw
  end

  def set_modified(bool, msg)
    @modified = bool
  end

  def modified?
    return @modified
  end

  def dump
    return @txt .buffer.text
  end
end

################################################################
## Combo widget for numeric values

class GtkNumericCombo < Gtk::Combo

  def initialize(min, max, default = min, step1 = 1, step2 = 1)
    super()
    @min, @max, @len = min, max, max .to_s .length
    @step1, @step2 = step1, step2
    set_min_max(min, max)
    set_number(default)

    self .entry .signal_connect('changed'){
      adjust_number
    }
  end

  def adjust_number
      s = self .entry .text
      i = s .to_i
      if (s !~ /^\d*$/) || (i != 0 &&  (i < @min || i > @max))
        set_number(i)
      end
  end

  def set_min_max(min, max)
    @min, @max, @len = min, max, max .to_s .length
    a = []
    while (min <= max)
      a << format("%0#{@len}d", min)
      min += @step1
    end
    self .set_popdown_strings(a)
    # self .set_usize(7 * @len + 30, 0)
    self .set_width_request(7 * @len + 30)
    self .entry .set_max_length(@len)
  end

  def set_min(i)
    set_min_max(i, @max)
  end

  def set_max(i)
    set_min_max(@min, i)
  end

  def set_number(i)
    i = i .to_i
    if (i < @min)
      i = @min
    elsif (i > @max)
      i = @max
    end
    self .entry .set_text(format("%0#{@len}d", i))
  end

  def dump
    return self .entry .text .to_i
  end
end

################################################################
## Spin buttons for numeric values.

class GtkNumericSpin < Gtk::SpinButton
  def initialize(from, to, default = from, step = 1, step2 = 5)
    adj = Gtk::Adjustment .new(from, from, to, step, step2, 0)
    len = to .to_s .length
    super(adj)
    self .set_wrap(true)
    # self .set_usize(7 * len + 30, 0)
    self .set_max_length(len)
    #self .set_update_policy(Gtk::SpinButton::UPDATE_ALWAYS)
    #self .set_update_policy(Gtk::SpinButton::UPDATE_IF_VALID)
    self .set_value(default)
  end

  def set_min(n)

  end

  def set_value(n)
    super(n .to_i)
  end

  alias set_number set_value

  def entry
    return self
  end

  def get_value
    return value_as_int .to_s
  end

  def dump
    value_as_int
  end
end

################################################################
## Entry with label

class GtkEntry < Gtk::HBox
  def initialize(label, text = '')
    super()
    lbl  = Gtk::Label .new(label)
    @ent  = Gtk::Entry .new .set_text(text)
    self .pack_start(lbl, false, false, 5)
    self .pack_start(@ent, true, true, 5)
  end

  def get_text
    text
  end
  def text
    return @ent .text
  end

  def set_text(str)
    @ent .set_text(MhcKconv::todisp(str))
    return self
  end

  def signal_connect(sig, &p)
    @ent .signal_connect(sig, &p)
  end
  alias dump get_text
end

################################################################
## Table of Toggle buttons.

class GtkToggleTable < Gtk::Table
  #FILL = Gtk::FILL | Gtk::EXPAND | Gtk::SHRINK
  FILL = Gtk::FILL
  NONE = Gtk::SHRINK

  def initialize(ys, xs, label, &p)
    @symbols = []
    super(ys, xs, true)

    for y in 0 .. ys - 1
      for x in 0 .. xs - 1
        lbl = label[xs * y + x]
        if lbl
          b = Gtk::ToggleButton .new(lbl .to_s)
          b .unset_flags(CAN_FOCUS) .set_border_width(0)
          @symbols << b
          b .signal_connect('toggled'){|w|
            p .call(w)
          }
          self .attach(b, x, x + 1, y, y + 1, FILL, FILL, 0, 0)
        end
      end
    end
  end

  def each_button
    @symbols .each{|b|
      yield(b, b .child .text)
    }
  end

  def dump
    ret = []
    @symbols .each{|b|
      if b .active?
        ret << b .child .text
      end
    }
    return ret .join(' ')
  end
end

################################################################
## Toolbar like widget.

class GtkButtonBar < Gtk::HBox
  TOP = Gtk::Window .new(Gtk::Window::TOPLEVEL)
  TOP .realize

  def initialize(btn)
    super(false, 0)
    @btn = {}

    btn .each{|xpm_tip|
      name, tip = *xpm_tip
      path = XPM_PATH + '/' + name + '.xpm'
      if !File .exists?(path)
        path = File .dirname($0) + '/xpm/' + name + '.xpm'
      end
      w = TOP .window
      s = TOP .style .bg(Gtk::STATE_NORMAL)
      pix, map = Gdk::Pixmap .create_from_xpm(w, s, path)
      xpm = Gtk::Image .new(pix, map)
      b = Gtk::Button .new() .unset_flags(CAN_FOCUS) .set_border_width(0)
      b .add(xpm)
      TIPS .set_tip(b, MhcKconv::todisp(tip), nil)
      self .pack_start(b, false, false, 0)
      @btn[name] = b
    }
  end

  def signal_connect(sig, &p)
    if sig =~ /(.*)-btn-clicked$/
      @btn[$1] .signal_connect('clicked', &p)
    else
      super
    end
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

### mhc-gtk.rb ends here
