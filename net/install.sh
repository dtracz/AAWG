
apt-get update
apt-get install -y wget unzip cmake libopencv-dev
ln -s /usr/include/opencv4/opencv2 /usr/include/opencv2

LIBTORCH="libtorch-shared-with-deps-1.6.0"
wget "https://download.pytorch.org/libtorch/cpu/$LIBTORCH%2Bcpu.zip"
unzip "$LIBTORCH+cpu.zip"
rm "$LIBTORCH+cpu.zip"

mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=$PWD/../libtorch -DCMAKE_BUILD_TYPE=Release ..
make
cd ..

