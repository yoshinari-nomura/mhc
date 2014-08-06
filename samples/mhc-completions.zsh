compdef _mhc mhc

_mhc () {
  local curcontext="$curcontext"
  local line state _opts

  _opts=("${(@f)$(mhc completions -- ${(Q)words[2,-1]})}")
  $_opts && return 0

  return 1
}
