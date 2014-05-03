# -*- coding: utf-8 -*-
require 'spec_helper'
require 'date'

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
  it "should parse a string and dump to the same string" do
    str = <<-EOF.gsub(/^\s+/, "")
      X-SC-Subject: Weekly Event on Monday and Thursday
      X-SC-Location: Office
      X-SC-Day: !20140410
      X-SC-Time: 12:40-14:10
      X-SC-Category: Work
      X-SC-Recurrence-Tag: TEST
      X-SC-Priority: 1
      X-SC-Cond: Mon Thu
      X-SC-Duration: 20140401-20140430
      X-SC-Alarm: 
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(Mhc::Event.parse(str).dump).to eq str + "\n"
  end

  it "should occur weekly on Monday and Thursday from 2014-04-01 to 2014-05-31" do
    # Weekly Event on Monday and Thursday from 2014-04-01 to 2014-04-30
    # with the exceptoin of 20140410 (Thu)
    #  1) 2014-04-03 Thu
    #  2) 2014-04-07 Mon
    #  3) 2014-04-10 Thu
    #  4) 2014-04-14 Mon
    #  5) 2014-04-17 Thu
    #  6) 2014-04-21 Mon
    #  7) 2014-04-24 Thu
    #  8) 2014-04-28 Mon -> out of range
    ev = Mhc::Event.parse <<-EOF.gsub(/^\s+/, "")
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
    expect(ev.occurrences(range:from..to).take(30).map{|occurrence| occurrence.date.to_s}).to eq \
      ["2014-04-03", "2014-04-07", "2014-04-14", "2014-04-17", "2014-04-21", "2014-04-24"]
  end

  it "should occur yearly on March 21" do
    ev = Mhc::Event.parse <<-EOF.gsub(/^\s+/, "")
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

  it "should show three enumerated days listed in X-SC-Day:" do
    ev = Mhc::Event.parse <<-EOF.gsub(/^\s+/, "")
      X-SC-Subject: Three enumerated events in X-SC-Day:
      X-SC-Day: 20140203 20140509 20140831
      X-SC-Duration: 20140101-20141231
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.take(30).map{|o| o.date.to_s}).to eq \
      ["2014-02-03", "2014-05-09", "2014-08-31"]
  end

  it "should show single day in X-SC-Day:" do
    ev = Mhc::Event.parse <<-EOF.gsub(/^\s+/, "")
      X-SC-Subject: Party
      X-SC-Day: 20140509
      X-SC-Time: 18:00-22:00
      X-SC-Cond:
      X-SC-Duration:
      X-SC-Record-Id: 1653B99D-DED2-4758-934F-B868BFCA9E9F
    EOF
    expect(ev.occurrences.take(30).map{|o| o.date.to_s}).to eq \
      ["2014-05-09"]
  end



  it "should show subject, time, date in X-SC-*:" do
    ev = Mhc::Event.parse <<-EOF.gsub(/^\s+/, "")
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
    ev = Mhc::Event.parse <<-EOF.gsub(/^\s+/, "")
      X-SC-Subject: TEST
      X-SC-Time:
      X-SC-Day: 20140203 20140509 20140831
      X-SC-Duration: 20140101-20141231
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.take(30).all? {|o| o.allday? }).to eq true
  end

  it "should return false when #allday? is called if X-SC-Time: is not blank" do
    ev = Mhc::Event.parse <<-EOF.gsub(/^\s+/, "")
      X-SC-Subject: TEST
      X-SC-Time: 10:00-12:00
      X-SC-Day: 20140203 20140509 20140831
      X-SC-Duration: 20140101-20141231
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.take(30).all? {|o| not o.allday? }).to eq true
  end

  it "should return almost infinit number of entries: from -50 years past to 50 years future, if X-SC-Duration: is empty" do
    ev = Mhc::Event.parse <<-EOF.gsub(/^\s+/, "")
      X-SC-Subject: New Year Day
      X-SC-Cond: 1 Jan
      X-SC-Duration:
      X-SC-Record-Id: FEDA4C97-21C2-46AA-A395-075856FBD5C3
    EOF
    expect(ev.occurrences.map{|o| o.date.to_s}.length).to eq 101
  end
end
