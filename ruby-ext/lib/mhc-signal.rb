# -*- coding: utf-8 -*-
### mhc-signal.rb
##
## Author:  Yoshinari Nomura <nom@quickhack.net>
##
## Created: 1999/07/16
## Revised: $Date: 2001/03/13 07:01:25 $
##

################################################################
################# common staff #################################
################################################################

################################################################
## message couduit

class SignalConduit
  def initialize
    @proc_table = {}
    @sd = 0
  end

  def signal_emit(sig, *arg)
    return 0 if @proc_table[sig] .nil?
    @proc_table[sig] .keys .sort .each{|k|
      @proc_table[sig][k] .call(*arg)
    }
  end

  def signal_connect(sig, &p)
    @sd += 1
    @proc_table[sig] = {} if @proc_table[sig] .nil?
    @proc_table[sig][@sd] = p
    return @sd
  end

  def signal_disconnect(sd)
    @proc_table .each_key{|sig|
      # @proc_table[sig][sd] = nil
      @proc_table[sig] .delete(sd)
    }
  end

  def dump
    @proc_table .each_key{|sig|
      print "(#{self}) #{sig} -> "  # if $DEBUG
      @proc_table[sig] .each_key{|sd|
        print @proc_table[sig][sd], " "
      }
      print "\n" # if $DEBUG
    }
  end
end

################################################################
## message couduit with alarm

class Alarm < SignalConduit

  def initialize
    super
    @now = Time .now .localtime
    @th  = Thread .new {
      while true
        tick
        sleep 3
      end
    }
    @th .abort_on_exception= true
  end

  def tick
    now = Time .now .localtime

    if (now != @now)
      signal_emit('sec-changed')
    end

    if (now .day != @now .day)
      signal_emit('day-changed')
    end

    if (now .min != @now .min)
      signal_emit('min-changed')
    end

    if (now .month != @now .month)
      signal_emit('month-changed')
    end

    @now = now
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

### mhc-signal.rb ends here
