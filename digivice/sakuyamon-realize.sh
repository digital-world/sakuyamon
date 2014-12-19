#!/bin/sh

# imitate possibly-missing readlink
readlink() {
  ls -l -- "$1" | sed -e "s/^.* -> //"
}

# Find absolute path to this script,
# resolving symbolic references to the end
# (changes the current directory):
D=`dirname "$0"`
F=`basename "$0"`
cd "$D"
while test -L "$F"; do
  P=`readlink "$F"`
  D=`dirname "$P"`
  F=`basename "$P"`
  cd "$D"
done

exec racket sakuyamon-realize.rkt ${1+"$@"}
