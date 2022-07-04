#!/bin/sh
#
# Helper script to fetch Emscripten under third-party

EMSDK_DIR=../../third-party/store/emsdk

if ! [ -x "$EMSDK_DIR"/emsdk_env.sh ]; then
    cat <<EOF
ERROR: Could not find emsdk_env.sh at $EMSDK_DIR.

Please install and activate emscripten:

  https://emscripten.org/docs/getting_started/downloads.html

Example:
  cd `dirname $EMSDK_DIR`
  git clone https://github.com/emscripten-core/emsdk.git
  cd emsdk/
  ./emsdk install latest
  ./emsdk activate latest

EOF
    exit 1
fi

if ! command -v python2 > /dev/null; then
    cat <<EOF
ERROR: python2 is not found in the PATH. You may fix it creating
a symbolic link other 2.x python version, e.g.,:

  ln -sf /usr/bin/python2.7 /usr/local/bin/python2

EOF
    exit 1
fi
# Pregenerate system libs (otherwise interaction with EMMAKEN_CFLAGS will make it fail)
( source "$EMSDK_DIR"/emsdk_env.sh > /dev/null; embuilder.py build dlmalloc pthreads_stub libc compiler-rt libc-extras )
