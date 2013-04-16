#!/usr/local/bin/ruby
## configure.rb -- Guess values for system-dependent variables.
##
## Author:  MIYOSHI Masanori <miyoshi@quickhack.net>
##          Yoshinari Nomura <nom@quickhack.net>
## Created: 2000/7/12
## Revised: $Date: 2006/12/18 06:50:14 $

$LOAD_PATH .unshift('.')
require 'mhc-make'
include MhcMake

################################################################a
## local configuralbe flags.

local_config_table = [
  ['--disable-palm', '@@MHC_DISABLE_PALM@@',
    GetoptLong::NO_ARGUMENT,
    "do not require pilot-link",
    ''],

  ['--pilot-link-lib', '@@MHC_PILOT_LINK_LIB@@',
    GetoptLong::REQUIRED_ARGUMENT,
    "=DIR    pilot-link lib in DIR",
    ''],

  ['--pilot-link-inc', '@@MHC_PILOT_LINK_INC@@',
    GetoptLong::REQUIRED_ARGUMENT,
    "=DIR    pilot-link header in DIR",
    ''],

  ['--with-mew', '@@MHC_WITH_MEW@@', GetoptLong::NO_ARGUMENT,
    "use mhc with Mew.",
    ''],

  ['--with-wl', '@@MHC_WITH_WL@@', GetoptLong::NO_ARGUMENT,
    "use mhc with Wanderlust.",
    ''],

  ['--with-gnus', '@@MHC_WITH_GNUS@@', GetoptLong::NO_ARGUMENT,
    "use mhc with Gnus.",
    ''],

  ['--with-cmail', '@@MHC_WITH_CMAIL@@', GetoptLong::NO_ARGUMENT,
    "use mhc with cmail.",
    ''],

  ['--with-icondir', '@@MHC_XPM_PATH@@', GetoptLong::REQUIRED_ARGUMENT,
    "=DIR  mhc icon directory.",
    '']
]

conf = MhcConfigure .new(local_config_table) .parse_argv

# XXX: ukai
if conf['@@MHC_XPM_PATH@@'] == ''
  conf['@@MHC_XPM_PATH@@'] = conf['@@MHC_LIBDIR@@'] + '/mhc/xpm'
end

################################################################
## command check

conf .search_command('ruby',         '@@MHC_RUBY_PATH@@',       false, true)
conf .search_command('emacs',        '@@MHC_EMACS_PATH@@',      false, false)
conf .search_command('emacs',        '@@MHC_FSF_EMACS_PATH@@',  false, false)
conf .search_command('xemacs',       '@@MHC_XEMACS_PATH@@',     false, false)
conf .search_command('make',         '@@MHC_MAKE_PATH@@',       false, true)

################################################################
## lib check

lib_search_path = ['/usr/local/lib', '/usr/local/pilot/lib',
  '/usr/lib', '/usr/pkg/lib']

inc_search_path = ['/usr/local/include', '/usr/local/pilot/include',
  '/usr/include/libpisock', '/usr/include', '/usr/pkg/include']

if conf['@@MHC_DISABLE_PALM@@'] == ''
  conf .search_library(lib_search_path,
                       'pisock',
                       'pi_socket',
                       '@@MHC_PILOT_LINK_LIB@@', false, true)
  conf .search_include(inc_search_path,
                       'pi-dlp.h',
                       '@@MHC_PILOT_LINK_INC@@', false, true)
end

################################################################
## replace keywords.

infile_list = [
  'mhc-sync.in:0755',
  'mhc2palm.in:0755',
  'palm2mhc.in:0755',
  'adb2mhc.in:0755',
  'gemcal.in:0755',
  'make.rb.in:0755',
  'today.in:0755',
  'emacs/make.rb.in:0755',
  'ruby-ext/lib/mhc-gtk.rb.in:0644',
  'ruby-ext/extconf.rb.in:0755'
]

if /cygwin|mingw32/ =~ RUBY_PLATFORM
  infile_list << 'mhc2ol.in:0755'
end

file = File .open('configure.log', 'w')
conf .each_macro{|key, val|
  file .print format("%-30s => %s\n", key, val)
}

conf .replace_keywords(infile_list)

print "In ruby-ext/\n"
Dir .chdir('ruby-ext')
#make_system('ruby extconf.rb')
make_system("#{conf['@@MHC_RUBY_PATH@@']} extconf.rb")

exit 0
