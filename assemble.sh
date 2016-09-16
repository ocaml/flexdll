#!/bin/bash

# Behaviour depends on OCaml < 4.03.x
if [ -z "$1" ] ; then
  # OCaml not being installed, so set VER to any value less than 403 to cause
  # the binaries to be installed.
  VER=402
else
  # Strip any extra data from the OCaml version number
  VER=${1%%+*}
  # Strip the last version component
  VER=${VER%.*}
  # Remove the remaining dot
  VER=${VER/./}
fi

if [ $VER -lt 403 ] ; then
  cat >> flexdll.install <<EOF
bin: [
  "flexlink.exe"
  "flexdll_$2.$3"
  "flexdll_initer_$2.$3"
]
EOF
  make MSVC_DETECT=0 CHAINS=$2 support
  unzip flexdll-bin-0.35.zip flexlink.exe
fi
