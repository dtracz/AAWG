
cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
project(ssd)

set(CMAKE_CXX_FLAGS_DEBUG_INIT "-Wall -g -O0")
set(CMAKE_CXX_FLAGS_RELEASE_INIT "-O3")

find_package(Torch REQUIRED)
# add dir with opencv *.so files
  link_directories(/usr/lib/x86_64-linux-gnu/)
 
add_library(ssd STATIC src/Net.cpp src/toolkit.cpp)
target_include_directories(ssd PUBLIC ./include)
target_link_libraries(ssd "${TORCH_LIBRARIES}")
# add required opencv shared objects
  target_link_libraries(ssd opencv_core)
  target_link_libraries(ssd opencv_imgcodecs)
  target_link_libraries(ssd opencv_imgproc)
set_property(TARGET ssd PROPERTY CXX_STANDARD 14)

