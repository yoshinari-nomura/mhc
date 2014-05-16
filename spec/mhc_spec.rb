require 'spec_helper'

describe Mhc do
  it 'should have a version number' do
    Mhc::VERSION.should_not be_nil
  end
end

describe Mhc::PropertyValue::Date do
  it "should parse 'today'" do
    expect(Mhc::PropertyValue::Date.parse_relative("today")).to eq(Date.today)
  end
end

describe Mhc::Event do
  before :all do
    ENV["MHC_TZINFO"] = 'UTC'
    RiCal::PropertyValue::DateTime.default_tzid = ENV["MHC_TZINFO"]
  end

  before :each do
    time_now = ::Time.utc(2014, 1, 1)
    ::Time.stub(:now).and_return(time_now)
  end

  it "should parse a string and dump to the same string" do
    str = <<-EOF.strip_heredoc
      X-SC-Subject: Weekly Event on Monday and Thursday
      X-SC-Location: Office
      X-SC-Day: !20140410
      X-SC-Time: 12:40-14:10
      X-SC-Category: Work
      X-SC-Mission-Tag: TEST-Mission
      X-SC-Recurrence-Tag: TEST
      X-SC-Cond: Mon Thu
      X-SC-Duration: 20140401-20140430
      X-SC-Alarm: 5 minutes
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
      X-SC-Sequence: 0

      this is description
    EOF
    ev = Mhc::Event.parse(str)
    expect(ev.dump).to eq str
  end

  it "should parse a string and dump to the same string even if all values are empty" do
    str = <<-EOF.strip_heredoc
      X-SC-Subject: 
      X-SC-Location: 
      X-SC-Day: 
      X-SC-Time: 
      X-SC-Category: 
      X-SC-Mission-Tag: 
      X-SC-Recurrence-Tag: 
      X-SC-Cond: 
      X-SC-Duration: 
      X-SC-Alarm: 
      X-SC-Record-Id: 
      X-SC-Sequence: 0

    EOF
    ev = Mhc::Event.parse(str)
    expect(ev.dump).to eq str
  end

  it "should occur weekly on Monday and Thursday from 2014-04-01 to 2014-04-30 with exceptoin of 2014-04-10 (Thu)" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Weekly Event on Monday and Thursday
      X-SC-Location: Office
      X-SC-Day: !20140410
      X-SC-Time: 12:40-14:10
      X-SC-Category: Work
      X-SC-Cond: Mon Thu
      X-SC-Duration: 20140401-20140430
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    from = Mhc::PropertyValue::Date.new(2014,04,7)
    to   = Mhc::PropertyValue::Date.new(2014,04,24)
    # range.first is not effective in narrowing the scan region so we will have "2014-04-03"
    expect(ev.occurrences(range:from..to).take(30).map{|occurrence| occurrence.date.to_s}).to eq \
      ["2014-04-03", "2014-04-07", "2014-04-14", "2014-04-17", "2014-04-21", "2014-04-24"]
  end

  it "should occur yearly on March 21" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Yearly Event on 21 March
      X-SC-Location: Office
      X-SC-Day:
      X-SC-Time: 12:40-14:10
      X-SC-Category: Work
      X-SC-Cond: Mar 21
      X-SC-Duration: 20140401-20200401
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.take(30).map{|o| o.date.to_s}).to eq \
      ["2015-03-21", "2016-03-21", "2017-03-21", "2018-03-21", "2019-03-21", "2020-03-21"]
  end

  it "should show single day in X-SC-Day:" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Party
      X-SC-Day: 20140509
      X-SC-Time: 18:00-22:00
      X-SC-Record-Id: 1653B99D-DED2-4758-934F-B868BFCA9E9F
    EOF
    expect(ev.occurrences.take(30).map{|o| o.date.to_s}).to eq \
      ["2014-05-09"]
  end

  it "should show three enumerated days listed in X-SC-Day:" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Three enumerated events in X-SC-Day:
      X-SC-Day: 20140203 20140509 20140831 20140901-20140902
      X-SC-Duration: 20140101-20141231
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.take(30).map{|o| o.to_s}).to eq \
      ["20140203", "20140509", "20140831", "20140901-20140902"]
  end

  it "should show three day's event even if scan range is started from the middle of event" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Three day's event
      X-SC-Day: 20140514-20140516
    EOF
    from = Mhc::PropertyValue::Date.new(2014, 5, 15)
    to   = Mhc::PropertyValue::Date.new(2014, 5, 16)
    expect(ev.occurrences(range:from..to).take(30).map{|o| o.to_s}).to eq \
      ["20140514-20140516"]
  end

  it "should produce a list of occurrences, and each occurrence has different date" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: TEST
      X-SC-Time: 10:00-12:00
      X-SC-Day: 20140203 20140509 20140831
      X-SC-Duration: 20140101-20141231
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.take(30).map{|o| "#{o.date} #{o.time_range} #{o.subject}"}).to eq \
      ["2014-02-03 10:00-12:00 TEST", "2014-05-09 10:00-12:00 TEST", "2014-08-31 10:00-12:00 TEST"]
  end

  it "should return true when #allday? is called if X-SC-Time: is blank" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: TEST
      X-SC-Time:
      X-SC-Day: 20140203 20140509 20140831
      X-SC-Duration: 20140101-20141231
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.take(30).all? {|o| o.allday? }).to eq true
  end

  it "should return false when #allday? is called if X-SC-Time: is not blank" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: TEST
      X-SC-Time: 10:00-12:00
      X-SC-Day: 20140203 20140509 20140831
      X-SC-Duration: 20140101-20141231
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.take(30).all? {|o| not o.allday? }).to eq true
  end

  it "should return almost infinit number of entries: 1970-1-1 to 50 years future, if X-SC-Duration: is empty" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: New Year Day
      X-SC-Cond: 1 Jan
      X-SC-Duration:
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.map{|o| o.date.to_s}.length).to eq 95
  end

  it "should return ``yearly by monthday'' icalendar rrule string" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: New Year Day
      X-SC-Cond: 1 Jan
      X-SC-Duration:
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.recurrence_condition.to_ics).to eq "FREQ=YEARLY;INTERVAL=1;WKST=MO;BYMONTH=1;BYMONTHDAY=1"
  end

  it "should return ``yearly by day'' icalendar rrule string" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Mother's Day
      X-SC-Cond: 2nd Sun May
      X-SC-Duration:
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.recurrence_condition.to_ics).to eq "FREQ=YEARLY;INTERVAL=1;WKST=MO;BYMONTH=5;BYDAY=2SU"
  end

  it "should return ``weekly'' icalendar rrule string" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Wednesday and Sunday Weekly event
      X-SC-Cond: Wed Sun
      X-SC-Duration: 20140401-20140424
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.recurrence_condition.to_ics(nil, ev.duration.last)).to eq "FREQ=WEEKLY;INTERVAL=1;WKST=MO;BYDAY=WE,SU;UNTIL=20140424"
  end

  it "should return icalendar VEVENT string with RRULE field" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Wednesday and Sunday Weekly event
      X-SC-Cond: Wed Sun
      X-SC-Category: Work
      X-SC-Location: Office
      X-SC-Duration: 20140401-20140424
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3

      this is description
    EOF
    expect(ev.to_ics).to eq <<-'EOF'.strip_heredoc
      BEGIN:VEVENT
      CREATED;VALUE=DATE-TIME:20140101T000000Z
      DTEND;VALUE=DATE:20140403
      DTSTART;VALUE=DATE:20140402
      DTSTAMP;VALUE=DATE-TIME:20140101T000000Z
      CATEGORIES:Work
      LAST-MODIFIED;VALUE=DATE-TIME:20140101T000000Z
      UID:FEDA4C97-21C2-46AA-A395-075856FBD5C3
      DESCRIPTION:this is description\n
      SUMMARY:Wednesday and Sunday Weekly event
      RRULE:FREQ=WEEKLY;INTERVAL=1;WKST=MO;BYDAY=WE,SU;UNTIL=20140424
      LOCATION:Office
      SEQUENCE:0
      END:VEVENT
    EOF
  end

  it "should return icalendar VEVENT string taking DTSTART from the first entry of X-SC-Day: and RDATE from remains" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: thee events on 20140203 20140509 20140831
      X-SC-Day: 20140203 20140509 20140831
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3

      this is description
    EOF
    expect(ev.to_ics).to eq <<-'EOF'.strip_heredoc
      BEGIN:VEVENT
      RDATE;VALUE=DATE:20140509,20140831
      CREATED;VALUE=DATE-TIME:20140101T000000Z
      DTEND;VALUE=DATE:20140204
      DTSTART;VALUE=DATE:20140203
      DTSTAMP;VALUE=DATE-TIME:20140101T000000Z
      LAST-MODIFIED;VALUE=DATE-TIME:20140101T000000Z
      UID:FEDA4C97-21C2-46AA-A395-075856FBD5C3
      DESCRIPTION:this is description\n
      SUMMARY:thee events on 20140203 20140509 20140831
      SEQUENCE:0
      END:VEVENT
    EOF
  end

  it "should return icalendar VEVENT string taking EXDATE from the entries of X-SC-Day: !YYYYMMDD" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: Wednesdays in 2014-04 (2, 9, 16, ..., 30) with exceptions of 2, 16
      X-SC-Cond: Wed
      X-SC-Duration: 20140401-20140430
      X-SC-Day: !20140402 !20140416
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3

      this is description
    EOF
    expect(ev.to_ics).to eq <<-'EOF'.strip_heredoc
      BEGIN:VEVENT
      EXDATE;VALUE=DATE:20140402,20140416
      CREATED;VALUE=DATE-TIME:20140101T000000Z
      DTEND;VALUE=DATE:20140403
      DTSTART;VALUE=DATE:20140402
      DTSTAMP;VALUE=DATE-TIME:20140101T000000Z
      LAST-MODIFIED;VALUE=DATE-TIME:20140101T000000Z
      UID:FEDA4C97-21C2-46AA-A395-075856FBD5C3
      DESCRIPTION:this is description\n
      SUMMARY:Wednesdays in 2014-04 (2\, 9\, 16\, ...\, 30) with exceptions of 2\, 16
      RRULE:FREQ=WEEKLY;INTERVAL=1;WKST=MO;BYDAY=WE;UNTIL=20140430
      SEQUENCE:0
      END:VEVENT
    EOF
  end

  it "should return icalendar VEVENT string taking EXDATE from the entries of X-SC-Day: !YYYYMMDD and X-SC-Time:" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: CS1
      X-SC-Time: 08:40-10:10
      X-SC-Category: Lecture
      X-SC-Day: !20140514
      X-SC-Cond: Wed
      X-SC-Duration: 20140409-20140723
      X-SC-Record-Id: 69CFD0DF-4058-425B-8C2B-40D81E6A2392
    EOF
    expect(ev.to_ics).to eq <<-'EOF'.strip_heredoc
      BEGIN:VEVENT
      EXDATE;VALUE=DATE-TIME:20140514T084000Z
      CREATED;VALUE=DATE-TIME:20140101T000000Z
      DTEND;VALUE=DATE-TIME:20140409T101000Z
      DTSTART;VALUE=DATE-TIME:20140409T084000Z
      DTSTAMP;VALUE=DATE-TIME:20140101T000000Z
      CATEGORIES:Lecture
      LAST-MODIFIED;VALUE=DATE-TIME:20140101T000000Z
      UID:69CFD0DF-4058-425B-8C2B-40D81E6A2392
      DESCRIPTION:
      SUMMARY:CS1
      RRULE:FREQ=WEEKLY;INTERVAL=1;WKST=MO;BYDAY=WE;UNTIL=20140723T084000Z
      SEQUENCE:0
      END:VEVENT
    EOF
  end

  it "should return icalendar VEVENT over 24h event" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: CS1
      X-SC-Time: 12:00-10:10
      X-SC-Day: 20140508-20140509
      X-SC-Record-Id: 69CFD0DF-4058-425B-8C2B-40D81E6A2392
    EOF
    expect(ev.to_ics).to eq <<-'EOF'.strip_heredoc
      BEGIN:VEVENT
      CREATED;VALUE=DATE-TIME:20140101T000000Z
      DTEND;VALUE=DATE-TIME:20140509T101000Z
      DTSTART;VALUE=DATE-TIME:20140508T120000Z
      DTSTAMP;VALUE=DATE-TIME:20140101T000000Z
      LAST-MODIFIED;VALUE=DATE-TIME:20140101T000000Z
      UID:69CFD0DF-4058-425B-8C2B-40D81E6A2392
      DESCRIPTION:
      SUMMARY:CS1
      SEQUENCE:0
      END:VEVENT
    EOF
  end

  it "should return icalendar VEVENT two day's allday event" do
    ev = Mhc::Event.parse <<-EOF.strip_heredoc
      X-SC-Subject: CS1
      X-SC-Day: 20140508-20140509
      X-SC-Record-Id: 69CFD0DF-4058-425B-8C2B-40D81E6A2392
    EOF
    expect(ev.to_ics).to eq <<-'EOF'.strip_heredoc
      BEGIN:VEVENT
      CREATED;VALUE=DATE-TIME:20140101T000000Z
      DTEND;VALUE=DATE:20140510
      DTSTART;VALUE=DATE:20140508
      DTSTAMP;VALUE=DATE-TIME:20140101T000000Z
      LAST-MODIFIED;VALUE=DATE-TIME:20140101T000000Z
      UID:69CFD0DF-4058-425B-8C2B-40D81E6A2392
      DESCRIPTION:
      SUMMARY:CS1
      SEQUENCE:0
      END:VEVENT
    EOF
  end
end
