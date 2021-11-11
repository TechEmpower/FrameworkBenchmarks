cmake_minimum_required(VERSION 3.5)

project(LightingIO VERSION 0.2.1 LANGUAGES C CXX)

# for generate ycm complete database
SET(CMAKE_CXX_STANDARD 14)

ADD_DEFINITIONS(
  -DUSE_TCMALLOC
  -DUSENONSTD_STRING_VIEW
  -DNET_ENABLE_REUSER_PORT
)
set(LTIO_BUILD_EXAMPLES OFF)
set(LTIO_BUILD_UNITTESTS OFF)

ADD_SUBDIRECTORY(ltio)

ADD_EXECUTABLE(benchmark_server
  http_benchmark.cc
)
TARGET_LINK_LIBRARIES(benchmark_server
  ltio
)
