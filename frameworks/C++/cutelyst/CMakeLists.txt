project(cutelyst_benchmarks)

cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)
if (POLICY CMP0043)
  cmake_policy(SET CMP0043 NEW)
endif()

find_package(Qt5 REQUIRED COMPONENTS Core Network Sql)
find_package(Cutelyst2Qt5 2.0.0 REQUIRED)

# Auto generate moc files
set(CMAKE_AUTOMOC ON)

# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Enable C++11 features
add_definitions(-std=c++11)

include_directories(
    ${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${Cutelyst2Qt5_INCLUDE_DIR}
)

file(GLOB_RECURSE TEMPLATES_SRC root/*)

add_subdirectory(src)
