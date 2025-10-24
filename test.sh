#!/bin/sh
for f in ./examples/*; do
    # Skip repl.metta
    ([ "$(basename "$f")" = "repl.metta" ] || [ "$(basename "$f")" = "gpt.metta" ] || [ "$(basename "$f")" = "mm2.metta" ] || [ "$(basename "$f")" = "morkspaceexec.metta" ] || [ "$(basename "$f")" = "morkspace.metta" ]) && continue
    echo "Running $f"
    output=$(sh run.sh "$f" | grep "is ")
    if ! echo "$output" | grep -q "✅" || echo "$output" | grep -q "❌"; then
        echo "Failure in $f: found $output"
        exit 1
    else
        echo "$output"
    fi
done

echo "Now testing examples with MORK:"

for f in ./examples/*; do
    # Skip repl.metta
    ([ "$(basename "$f")" != "morkspaceexec.metta" ] && [ "$(basename "$f")" != "morkspace.metta" ]) && continue
    echo "Running $f"
    output=$(sh run.sh "$f" mork | grep "is ")
    if ! echo "$output" | grep -q "✅" || echo "$output" | grep -q "❌"; then
        echo "Failure in $f: found $output"
        exit 1
    else
        echo "$output"
    fi
done

exit 0
