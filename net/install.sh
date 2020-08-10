
# apt-get update

LIBTORCH="libtorch-shared-with-deps-1.6.0"
wget "https://download.pytorch.org/libtorch/cpu/$LIBTORCH%2Bcpu.zip"
unzip "$LIBTORCH+cpu.zip"

mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=/workspace/libtorch -DCMAKE_BUILD_TYPE=Release ..
make
cd ..

