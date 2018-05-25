project(cutelyst_benchmarks)

cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)
if (POLICY CMP0043)
  cmake_policy(SET CMP0043 NEW)
endif()

find_package(Qt5 5.6.0 REQUIRED COMPONENTS Core Network Sql)
find_package(Cutelyst2Qt5 2.0.0 REQUIRED)

# Auto generate moc files
set(CMAKE_AUTOMOC ON)

# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(
    ${CMAKE_SOURCE_DIR}
    ${Cutelyst2Qt5_INCLUDE_DIR}
)

file(GLOB_RECURSE TEMPLATES_SRC root/*)
file(GLOB_RECURSE cutelyst_benchmarks_SRCS *.cpp *.h)

set(cutelyst_benchmarks_SRCS
    ${cutelyst_benchmarks_SRCS}
    ${TEMPLATES_SRC}
)

# Create the application
add_library(cutelyst_benchmarks SHARED ${cutelyst_benchmarks_SRCS})

# Link to Cutelyst
target_link_libraries(cutelyst_benchmarks
    Cutelyst::Core
    Cutelyst::Utils::Sql
    Cutelyst::View::Grantlee
    Qt5::Core
    Qt5::Network
    Qt5::Sql
)

