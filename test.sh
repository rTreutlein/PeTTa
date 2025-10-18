#!/bin/sh
for f in ./examples/*; do
    # Skip repl.metta
    [ "$(basename "$f")" = "repl.metta" ] && continue

    echo "Running $f"
    output=$(sh run.sh "$f" | grep "is ")
    if echo "$output" | grep -q "❌"; then
        echo "Failure in $f: found $output"
        exit 1
    else
        echo $output
    fi
done

exit 0
