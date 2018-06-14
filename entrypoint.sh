#!/bin/bash

TRACE_FILE="/output/$(basename $1 .ml).traces"

# Get setup
cd /app 
ulimit -s unlimited 
eval `opam config env` &> /dev/null

# Add in target file
cp /target/$1 ./driver/main.ml &> /dev/null
echo "[lsee] Building symbolic executor"
jbuilder build ./driver/main.exe 

# Generate traces
echo "[lsee] Generating traces..."
./_build/default/driver/main.exe &> "$TRACE_FILE"

echo "[lsee] Generated $(wc -l $TRACE_FILE | awk '{print $1}') traces!"
echo "[lsee] Stored in '$TRACE_FILE'"
echo "[lsee] Trace generation completed."
