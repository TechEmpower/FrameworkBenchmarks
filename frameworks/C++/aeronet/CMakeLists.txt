cmake_minimum_required(VERSION 3.23)
project(techempower_aeronet CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Fetch aeronet from GitHub
include(FetchContent)

set(AERONET_GIT_TAG "main" CACHE STRING "Git tag/branch for aeronet")

FetchContent_Declare(
  aeronet
  GIT_REPOSITORY https://github.com/sjanel/aeronet.git
  GIT_TAG ${AERONET_GIT_TAG}
  GIT_SHALLOW TRUE
)

# Configure aeronet with minimal features for TechEmpower
set(AERONET_ENABLE_GLAZE ON CACHE BOOL "" FORCE)
set(AERONET_ENABLE_ASYNC_HANDLERS OFF CACHE BOOL "" FORCE)
set(AERONET_ENABLE_HTTP2 OFF CACHE BOOL "" FORCE)
set(AERONET_ENABLE_BROTLI OFF CACHE BOOL "" FORCE)
set(AERONET_ENABLE_ZLIB OFF CACHE BOOL "" FORCE)
set(AERONET_ENABLE_ZSTD OFF CACHE BOOL "" FORCE)
set(AERONET_ENABLE_OPENSSL OFF CACHE BOOL "" FORCE)
set(AERONET_ENABLE_OPENTELEMETRY OFF CACHE BOOL "" FORCE)
set(AERONET_ENABLE_WEBSOCKET OFF CACHE BOOL "" FORCE)
set(AERONET_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(AERONET_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
set(AERONET_BUILD_BENCHMARKS OFF CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(aeronet)

# TechEmpower benchmark executable
add_executable(benchmark_techempower main.cpp)
target_link_libraries(benchmark_techempower PRIVATE aeronet)

