#sh build.sh $1
#swipl -q -s nars.pl -g "set_prolog_flag(backtrace,true), trace, main" -t halt

swipl -q -s ./src/main.pl -g main -t halt -- $1
