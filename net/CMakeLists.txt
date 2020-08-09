cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
project(ssd)

find_package(Torch REQUIRED)
# add dir with opencv *.so files
  link_directories(/usr/lib/x86_64-linux-gnu/)

add_executable(ssd.x ssd.cpp)
target_link_libraries(ssd.x "${TORCH_LIBRARIES}")
# add required opencv shared objects
  target_link_libraries(ssd.x opencv_core)
  target_link_libraries(ssd.x opencv_imgcodecs)
  target_link_libraries(ssd.x opencv_imgproc)
set_property(TARGET ssd.x PROPERTY CXX_STANDARD 14)
