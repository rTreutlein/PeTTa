cd /home/nartech/MORK/kernel/
RUSTFLAGS="-C target-cpu=native" cargo build -p mork_ffi --release
nm -D /home/nartech/MORK/target/release/libmork_ffi.so | grep rust_mork

cd /home/nartech/PeTTa/
gcc -shared -fPIC -o mylib.so main.c $(pkg-config --cflags --libs swipl)
