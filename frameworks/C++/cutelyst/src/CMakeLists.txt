cmake_minimum_required(VERSION 3.6.0 FATAL_ERROR)

project(cutelyst_benchmarks LANGUAGES CXX)

cmake_policy(SET CMP0069 NEW)

find_package(Qt5 5.6.0 REQUIRED COMPONENTS Core Network Sql)
find_package(ASqlQt5 0.43.0 REQUIRED)
find_package(Cutelyst3Qt5 3.1 REQUIRED)
find_package(Cutelee6Qt5 6.0.0 REQUIRED)
find_package(PostgreSQL REQUIRED)
find_package(mimalloc 1.0)

# Auto generate moc files
set(CMAKE_AUTOMOC ON)

# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

file(GLOB_RECURSE TEMPLATES_SRC root/*)

set(cutelyst_benchmarks_SRCS
    cutelyst-benchmarks.cpp
    cutelyst-benchmarks.h
    databaseupdatestest.cpp
    databaseupdatestest.h
    fortunetest.cpp
    fortunetest.h
    jsontest.cpp
    jsontest.h
    multipledatabasequeriestest.cpp
    multipledatabasequeriestest.h
    plaintexttest.cpp
    plaintexttest.h
    root.cpp
    root.h
    singledatabasequerytest.cpp
    singledatabasequerytest.h
    ${TEMPLATES_SRC}
)

# Create the application
add_library(cutelyst_benchmarks SHARED ${cutelyst_benchmarks_SRCS})

# Link to Cutelyst
target_link_libraries(cutelyst_benchmarks
    PUBLIC
    Cutelyst::Core
    Cutelyst::Utils::Sql
    Cutelyst::View::Cutelee
    Qt5::Core
    Qt5::Network
    Qt5::Sql
    ASqlQt5::Core
)

add_executable(cutelyst-benchmarks ${cutelyst_benchmarks_SRCS} main.cpp)
target_link_libraries(cutelyst-benchmarks
    PUBLIC
    Cutelyst::Core
    Cutelyst::Server
    Cutelyst::Utils::Sql
    Cutelyst::View::Cutelee
    Qt5::Core
    Qt5::Network
    Qt5::Sql
    ASqlQt5::Core
)
if (mimalloc_FOUND)
    target_link_libraries(cutelyst-benchmarks PUBLIC mimalloc)
endif ()
