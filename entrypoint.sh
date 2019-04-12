#!/bin/bash

BASE_FILE="/output/$(basename $1 .ml)"
TRACE_FILE="/output/$(basename $1 .ml).traces"
ERRS_FILE="/output/$(basename $1 .ml).errs.log"
BUILD_FILE="/output/$(basename $1 .ml).build.log"

sudo touch "$TRACE_FILE"
sudo chown opam "$TRACE_FILE"
sudo touch "$ERRS_FILE"
sudo chown opam "$ERRS_FILE"
sudo touch "$BUILD_FILE"
sudo chown opam "$BUILD_FILE"

# Get setup
cd /app 
ulimit -s unlimited 
eval `opam config env` &> /dev/null

# Add in target file
cp /target/$1 ./driver/main.ml &> /dev/null
echo "[lsee] Building symbolic executor"
jbuilder build ./driver/main.exe &> "$BUILD_FILE"

# Generate traces
echo "[lsee] Generating traces..."
./_build/default/driver/main.exe 2> "$ERRS_FILE" > "$TRACE_FILE"

echo "[lsee] Generated $(wc -l $TRACE_FILE | awk '{print $1}') traces!"
echo "[lsee] Stored in '$TRACE_FILE'"
echo "[lsee] Trace generation completed."
