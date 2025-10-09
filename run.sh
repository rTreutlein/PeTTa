LD_PRELOAD=../MORK/target/release/libmork_ffi.so swipl --stack_limit=8g -q -s ./src/main.pl -g main -- $*
