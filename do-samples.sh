#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

rm -rf $DIR/outputs/$1
mkdir -p $DIR/outputs/$1

docker run \
  -it \
  --rm \
  --entrypoint bash \
  -v $DIR/$1-raw.traces.gz:/traces.gz \
  -v $DIR/sample.py:/app/sample.py \
  -v $DIR/word-threshold.py:/app/threshold.py \
  -v $DIR/outputs/$1:/output \
  python:3.7 \
    -c "\
    set -ex ; \
    cat /traces.gz \
      | gzip -cd \
      | python3 /app/threshold.py \
    > /traces.txt; \
    cat /traces.txt \
      | SAMPLE=RANDOM python3 /app/sample.py \
      | gzip - > /output/$1-s-random.traces.gz; \
    cat /traces.txt \
      | SAMPLE=DIVERSITY python3 /app/sample.py \
      | gzip - > /output/$1-s-diversity.traces.gz; \
    cat /traces.txt \
      | SAMPLE=NONE python3 /app/sample.py \
      | gzip - > /output/$1-s-none.traces.gz \
    "
