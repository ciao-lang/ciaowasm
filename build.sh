#!/bin/sh

if ! [ -x "$(command -v emcc)" ]; then
    cat <<EOF
ERROR: Could not find 'emcc' in the path. 

Please install and activate emscripten:

  https://emscripten.org/docs/getting_started/downloads.html

Make sure that your installation is enabled in your current shell with:

  # (from your emsdk directory)
  source ./emsdk_env.sh

EOF
    exit 1
fi

# Build and install
ciao build ciaowasm
ciao build --grade=wasm ciaowasm
ciao install --grade=wasm -x ciaowasm
