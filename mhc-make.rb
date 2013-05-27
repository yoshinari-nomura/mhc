## mhc-make.rb -- Installer for Ruby scripts.
##
## Author:  MIYOSHI Masanori <miyoshi@quickhack.net>
##          Yoshinari Nomura <nom@quickhack.net>
## Created: 2000/7/12
## Revised: $Date: 2001/04/10 01:16:21 $

require 'rbconfig'
require 'mkmf'
require 'fileutils'
require 'kconv'
require 'getoptlong'

def File .which(command)
  bindir = CONFIG['bindir']

  if bindir and File .exist?(path = (bindir + '/' + command))
    return path
  end

  ENV['PATH'] .split(':') .each{|dir|
    path = dir + '/' + command
    if File .exist?(path)
      return path
    end
  }
  return nil
end

module MhcMake

  def default()
    if File .exists?('Makefile')
      make_system("make")
    else
      ## print "Nothing to do in #{Dir .pwd}.\n"
    end
    process_subdirs()
  end

  def clean
    if File .exists?('Makefile')
      make_system("make clean")
    else
      Dir .foreach('.'){|src_file|
        if src_file =~ /\.in$/ or src_file == 'Makefile' or
            src_file == 'make.rb'

          dst_file = src_file .sub(/\.in$/, '')
          if File .exist?(dst_file)
            File .delete(dst_file)
            print "removing: " + dst_file + "\n";
          end
        end
      }
    end
    process_subdirs()
  end


  def make_system(*commandline)
    commandline = commandline .join(' ')
    echo_flag, exit_flag = true, true

    if commandline =~ /^-(.*)/
      commandline = $1
      exit_flag = false
    end

    if commandline =~ /^@(.*)/
      commandline = $1
      echo_flag = false
    end

    print commandline, "\n"  if echo_flag

    system(commandline)
    result = $? >> 8
    if result != 0 and exit_flag
      exit result
    end
  end

  def process_subdirs()
    target = ARGV .join(' ')

    Dir .foreach('.'){|entry|
      if entry !~ /^\./ and File .directory?(entry)
        if File .exists?("#{entry}/make.rb")
          print "Making #{target} in #{File .expand_path(entry)}\n"
          cd = Dir .pwd()
          Dir .chdir(File .expand_path(entry))
          make_system('ruby', 'make.rb', *ARGV)
          Dir .chdir(cd)

        elsif File .exists?("#{entry}/Makefile")
          print "Making #{target} in #{File .expand_path(entry)}\n"
          cd = Dir .pwd()
          Dir .chdir(File .expand_path(entry))
          make_system('make', *ARGV)
          Dir .chdir(cd)
        end
      end
    }
  end

  def install
    if File .exists?('Makefile')
      make_system("make", "install")
    else
      INSTALL_FILES .each{|filename_mode_dir|
        filename, mode, dir = filename_mode_dir .split(':')
        FileUtils .makedirs(dir) if ! File .directory?(dir)
        FileUtils .install(filename, dir, {:mode => mode .oct, :verbose => true})
      }
    end
    process_subdirs()
  end

  def print_usage()
    print "Usage: make.rb [target]\ntarget can be none, install or clean.\n"
  end

  def doit
    if (ARGV .size == 0)
      default()
    else
      case ARGV[0]
      when "install"
        install()
      when "clean"
        clean()
      else
        print_usage();
        exit(1);
      end
    end
  end
end

class MhcConfigTable
  include RbConfig
  # ['--kcode', '@@MHC_KCODE@@', GetoptLong::OPTIONAL_ARGUMENT, usage, default]

  DEFAULT_CONFIG_TABLE = [
    ['--help', '@@MHC_HELP@@', GetoptLong::NO_ARGUMENT,
      "print this message",
      ''],

    ['--kcode', '@@MHC_KCODE@@', GetoptLong::REQUIRED_ARGUMENT,
      "=CODE  kanji code (EUC, JIS, SJIS)",
      (/cygwin|mingw32|os2_emx|sharp-human/ =~ RUBY_PLATFORM) ? 'SJIS' : 'EUC'
    ],

    ['--bindir', '@@MHC_BINDIR@@', GetoptLong::REQUIRED_ARGUMENT,
      "=DIR   user executables go to  DIR",
      CONFIG["bindir"]],

    ['--with-ruby', '@@MHC_RUBY_PATH@@', GetoptLong::REQUIRED_ARGUMENT,
      "=PATH  absolute path of ruby executable",
      ''],

    ['--libdir', '@@MHC_LIBDIR@@', GetoptLong::REQUIRED_ARGUMENT,
      "=DIR   Ruby script libraries go to DIR",
      CONFIG["rubylibdir"]],

    ['--with-emacs', '@@MHC_EMACS_PATH@@', GetoptLong::REQUIRED_ARGUMENT,
      "=PATH  absolute path of emacs/xemacs executable",
      ''],

    ['--with-lispdir', '@@MHC_LISPDIR@@', GetoptLong::REQUIRED_ARGUMENT,
      "=DIR   emacs lisp files go to DIR.",
      ''],

    ['--with-xemacs-pkgdir', '@@MHC_XEMACS_PACKAGE_DIR@@',
      GetoptLong::REQUIRED_ARGUMENT,
      '=DIR   emacs lisp files as package go to DIR.',
      ''],

    ['--with-emacs-addpath', '@@MHC_EMACS_ADD_PATH@@',
      GetoptLong::REQUIRED_ARGUMENT,
      '=PATH  add colon separated dirs list, to `load-path\'',
      '']
  ]

  def initialize(config_table = [])
    @config_table = DEFAULT_CONFIG_TABLE + config_table
  end

  def getopt_table
    return @config_table .collect{|ary| [ary[0], ary[2]]}
  end

  def usage_string
    opt_ary, ret, opt_name_max_length = [], '', 0

    @config_table .each{|ary|
      opt_name, opt_usage = ary[0], ary[-2]

      if opt_usage =~ /^(=\S+)\s+(.*)/
        opt_name  += $1
        opt_usage  = $2
      end

      if (opt_name_max_length < opt_name .length)
        opt_name_max_length = opt_name .length
      end
      opt_ary << [opt_name, opt_usage]
    }
    opt_ary .each{|opt|
      ret += format("  %-#{opt_name_max_length}s  %s\n", opt[0], opt[1])
    }
    return ret
  end

  def macro_hash
    hash = {}
    @config_table .each{|ary| hash[ary[1]] = ary[-1]}
    return hash
  end

  def macro_name(option_string)
    @config_table .each{|ary|
      return ary[1] if ary[0] == option_string
    }
    return nil
  end

  def option_name(macro_name)
    @config_table .each{|ary|
      return ary[0] if ary[1] == macro_name
    }
    return nil
  end
end

class MhcConfigure
  include RbConfig

  def initialize(local_config_table = [])
    @macros = {}
    @config_table = MhcConfigTable .new(local_config_table)

    ## import macros from rbconfig.rb
    CONFIG .each{|key, val| @macros[make_macro_name(key)] = val}

    ## import macros from configure table
    @macros .update(@config_table .macro_hash)

    ## set useful macros.
    @macros['@@MHC_RUBY_VERSION@@'] =
      RUBY_VERSION .split('.') .collect{|i| format("%02d", i)} .join('')
    @macros['@@MHC_TOPDIR@@'] = Dir .pwd
  end

  def usage
    STDERR .print "usage: ruby configure.rb [options]\n"
    STDERR .print @config_table .usage_string
  end

  ## parse ARGV and set corresponding macros.
  def parse_argv()
    parser = GetoptLong .new()
    parser .set_options(*@config_table .getopt_table)
    begin
      parser .each_option do |name, arg|
        if name == '--help'
          usage()
          exit(0)
        else
          @macros[@config_table .macro_name(name)] = (arg == '' ? '1' : arg)
        end
      end
    rescue
      usage()
      exit(1)
    end
    return self
  end

  def [](name);         @macros[name];         end
  def []=(name, val);   @macros[name] = val;   end

  def macro(name)
    return @macros[name]
  end

  def set_macro(name, val)
    return @macros[name] = val
  end

  def add_macro(name, val)
    return @macros[name] += val
  end

  def each_macro()
    @macros .each do |key, val|
      yield(key, val)
    end
  end

  ## replace keywords in files. in_file_list:
  ## infile_list is a array of 'filename:mode'
  ## such like ['mhc-sync.in:0755', 'mhc2palm.in:0755' ...]
  def replace_keywords(in_file_list)
    in_file_list .each{|src_file_and_mode|
      src_file, mode = src_file_and_mode .split(':')
      dst_file = src_file .sub(/\.in$/, '')
      print "creating #{dst_file} ..."
      replace_keywords1(src_file, dst_file , @macros, mode .oct)
      print "done.\n"
    }
  end

  ## find header file, add to '@@MHC_CFLAGS@@', set macroname.
  def search_include(search_path, header_file, macroname, force, abort)

    if @macros[macroname] and @macros[macroname] != '' and !force
      search_path = [@macros[macroname]]
    end

    cflags, found_inc_path = $CFLAGS, nil

    search_path .each{|inc_path|
      print "In #{inc_path} .. "
      $CFLAGS  = "-I#{inc_path}"

      if have_header(header_file)
        found_inc_path = inc_path
        # avoiding ruby 1.4.3 bug.
        $defs .push($defs .pop .sub!(/-DHAVE_PI-DLP_H/, '-DHAVE_PI_DLP_H'))
        break
      end
    }
    $CFLAGS  = cflags

    if found_inc_path
      @macros['@@MHC_CFLAGS@@'] += " -I#{found_inc_path} "
      @macros[macroname] = found_inc_path
    elsif abort
      search_abort(header_file, macroname)
    end
    return found_inc_path
  end

  ## find library file, add to '@@MHC_LDFLAGS@@', set macroname.
  def search_library(search_path, libname, funcname, macroname, force, abort)

    if @macros[macroname] and @macros[macroname] != '' and !force
      search_path = [@macros[macroname]]
    end

    ldflags, found_lib_path = $LDFLAGS, nil

    search_path .each{|lib_path|
      print "In #{lib_path} .. "
      $LDFLAGS = "-L#{lib_path}"
      if have_library(libname, funcname)
        found_lib_path = lib_path
        break
      end
    }
    $LDFLAGS = ldflags

    if found_lib_path
      @macros['@@MHC_LDFLAGS@@'] += " -L#{found_lib_path} "
      @macros[macroname] = found_lib_path
    elsif abort
      search_abort(libname, macroname)
    end
    return found_lib_path
  end

  ## find command and set macro name.
  ## if force is true, overwrite the macro even if previously set.
  ## if abort is true, stop and exit.
  def search_command(command, macroname, force, abort)
    path = @macros[macroname]

    if (!path) or path == '' or force
      if path = File .which(command)
        @macros[macroname] = path
      end
    end

    if path and File .executable?(path)
      print "#{command} is .. #{path}\n"
    else
      search_abort(command, macroname) if abort
      return nil
    end
  end

  ################################################################a
  private

  def search_abort(missing, macroname)
    if option_name = @config_table .option_name(macroname)
      helping_option = "#{option_name} or --help"
    else
      helping_option = "--help"
    end
    STDERR .print "######################################################\n"
    STDERR .print "Fatal: could not find #{missing} .. aborting.\n"
    STDERR .print "Fatal: option #{helping_option} may help you.\n"
    STDERR .print "######################################################\n"
    exit(1)
  end

  def make_macro_name(name)
    return '@@MHC_' + name .sub(/^-*/, '') .tr('a-z-', 'A-Z_') + '@@'
  end

  def replace_keywords1(src_file_name, dst_file_name, keywords, mode = nil)
    src_file  = File .open(src_file_name, "r") or die "#{$!}\n"
    dst_file  = File .open(dst_file_name, "w") or die "#{$!}\n"

    src_contents = src_file .gets(nil); src_file .close
    src_contents .force_encoding("ASCII-8BIT") if RUBY_VERSION .to_f >= 1.9
    keywords .each{|key, val| src_contents .gsub!(key, val)}

    if src_contents =~ /(@@MHC_[a-z\d_]+@@)/in
      STDERR .print "Warn: keyword #{$1} was remiained in #{dst_file_name}.\n"
    end

    dst_file << src_contents
    dst_file .close
    File .chmod(mode, dst_file_name) if mode
  end

end

### Copyright Notice:

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

### mhc-make.rb ends here
