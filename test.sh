#!/bin/sh
set -e

runset() {
  echo "== ${2:-Normal run} =="
  for f in ./examples/*.metta; do
    b=$(basename "$f")
    case "$1/$b" in
      normal/repl.metta|normal/gpt.metta|normal/mm2.metta|normal/morkspace*.metta) continue ;;
      mork/*)
        case "$b" in
          morkspace.metta|morkspaceexec.metta) ;;
          *) continue ;;
        esac ;;
    esac
    echo "Running $b"
    o=$(sh run.sh "$f" ${1#mork} | grep "is " || true)
    if echo "$o" | grep -q "❌"; then
      echo "❌ $b: $o"; exit 1
    fi
    if ! echo "$o" | grep -q "✅"; then
      echo "⚠️  $b: $o"; exit 1
    fi
    echo "$o"
  done
}

runset normal
runset mork "MORK mode"
echo "✅ All tests passed."
exit 0
