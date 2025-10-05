cd /home/nartech/MORK/kernel/
RUSTFLAGS="-C target-cpu=native" cargo build -p mork_ffi --release

cd /home/nartech/PeTTa/
gcc -fPIC -shared -o mylib.so main.c \
  $(pkg-config --cflags swipl) \
  -L/home/nartech/MORK/target/release \
  -Wl,--no-as-needed \
  -Wl,-rpath,/home/nartech/MORK/target/release \
  -l:libmork_ffi.so \
  $(pkg-config --libs swipl)
nm -D /home/nartech/MORK/target/release/libmork_ffi.so | grep rust_mork






#gcc -shared -fPIC -o mylib.so main.c \
#    $(pkg-config --cflags --libs swipl)



#NOOO:
#swipl-ld -shared -o mylib.so main.c \
#  -Wl,--no-as-needed \
#  -L/home/nartech/MORK/target/release -lmork_ffi \
#  -Wl,-rpath,/home/nartech/MORK/target/release
