#!/bin/sh

for f in ./examples/*; do
    echo "Running $f"
    sh run.sh "$f" | grep "is "
done
