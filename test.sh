#!/bin/sh
for f in ./examples/*; do
    [ "$(basename "$f")" = "repl.metta" ] && continue
    echo "Running $f"
    sh run.sh "$f" | grep "is "
done
