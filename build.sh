#!/bin/sh

# Helper build script for ciaowasm
#
# Use "FORCE_ENG_REBUILD=yes ./build.sh" to force engine rebuild.

# TODO: merge with ciao_playground/build.sh

# ---------------------------------------------------------------------------

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
      cd "$d";done;cd "$(dirname "$e")";pwd -P)

set -e

# ---------------------------------------------------------------------------

ciaoroot=$_base/../.. # TODO: only works for monorepo!

has_bundle() {
    ciao info "$1" > /dev/null 2>&1
}

# TODO: unused
bundle_dir() {
    ciao info "$1" | sed -n '/src:/s/^[[:space:]]*src:[[:space:]]*//p' 
}

# ---------------------------------------------------------------------------

check_bundle() { # bundle
    if ! has_bundle "$1"; then
        cat <<EOF
ERROR: Cannot locate the '$1' bundle.

Make sure that Ciao is installed and this bundle enabled and
activated.

EOF
        exit 1
    fi
}

check_emcc() {
    if ! [ -x "$(command -v emcc)" ]; then
        cat <<EOF
ERROR: Could not find 'emcc' in the path. 

Please install and activate emscripten (>= 4.0.9):

  https://emscripten.org/docs/getting_started/downloads.html

Make sure that your installation is enabled in your current shell with:

  # (from your emsdk directory)
  source ./emsdk_env.sh

EOF
        exit 1
    fi
}

# ---------------------------------------------------------------------------
# Install ciaowasm

build_ciaowasm() {
    check_bundle ciaowasm
    check_emcc
    # (build and install ciaowasm)
    if [ x"$FORCE_ENG_REBUILD" = x"yes" ]; then # make sure engine is rebuilt
        if [ -r "$ciaoroot"/core/engine/ciaoengine_common.pl ]; then
            touch "$ciaoroot"/core/engine/ciaoengine_common.pl
            rm -rf "$ciaoroot"/build/eng/ciaoengwasm/
        else
            cat <<EOF
Cannot locate ciaoengine_common.pl
EOF
            exit 1
        fi
    fi
    ciao build --bin ciaowasm
    ciao build --grade=wasm ciaowasm
    ciao install --grade=wasm -x ciaowasm
}

# ---------------------------------------------------------------------------

build_ciaowasm
