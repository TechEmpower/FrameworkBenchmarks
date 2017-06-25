cmake_minimum_required(VERSION 2.6)
include(CMakeToolsHelpers OPTIONAL)
include("common.cmake")

project(octane C CXX)

set(CMAKE_BUILD_TYPE RelWithDebInfo)
set (CMAKE_CXX_STANDARD 14)

add_definitions(-std=gnu99)
#add_definitions(-msse4.1)
add_definitions(-mavx)
add_definitions(-pedantic)
add_definitions(-O3)
add_definitions(-Wall)
add_definitions(-Wextra)
add_definitions(-Wcast-align)
add_definitions(-w)

# ----------------------------------------
# Benchmark service
# ----------------------------------------
file(GLOB_RECURSE TECHEMPOWER_BENCHMARKS_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/octane/lib/sds/*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/octane/lib/sds/*.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/src/*.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp)

list(SORT TECHEMPOWER_BENCHMARKS_SOURCES)
create_source_group("Source Files" "${CMAKE_CURRENT_SOURCE_DIR}/src" ${TECHEMPOWER_BENCHMARKS_SOURCES})
include_directories(${CMAKE_SOURCE_DIR}/lib/octane/lib/libuv/include)
include_directories(${CMAKE_SOURCE_DIR}/lib/octane/lib/sds)
include_directories(${CMAKE_SOURCE_DIR}/lib/octane/lib/rapidjson/include)
include_directories(${CMAKE_SOURCE_DIR}/lib/octane/include)
include_directories(${CMAKE_SOURCE_DIR}/include)

find_package(Threads REQUIRED)

add_executable (techempower_benchmarks
    ${TECHEMPOWER_BENCHMARKS_SOURCES})

# Libraries to link in reverse order because that's what ld requires.
target_link_libraries (techempower_benchmarks
    ${CMAKE_SOURCE_DIR}/lib/octane/build/liboctane.a
    ${CMAKE_SOURCE_DIR}/lib/octane/lib/libuv/.libs/libuv.a
    ${CMAKE_THREAD_LIBS_INIT})

if (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    target_link_libraries (techempower_benchmarks rt)
endif (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
