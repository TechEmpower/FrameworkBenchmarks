cmake_minimum_required(VERSION 3.15)
project(suil-bench VERSION 0.1.0 LANGUAGES C CXX)

set(SUIL_BASE_PATH "" CACHE STRING "The root path when SCF is installed")
if (SUIL_BASE_PATH)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${SUIL_BASE_PATH}/lib/cmake)
endif()

option(SUIL_BENCH_DEV "Build development binaries" OFF)
if (SUIL_BENCH_DEV)
    set(SUIL_BENCH_DEV 1)
else()
    set(SUIL_BENCH_DEV 0)
endif()

# Provides Suil* functions
include(Suil)

# Start a suil project
SuilStartProject(suil-bench
        EXPORTS ON
        SCC_SOURCES src/app.scc)

# Create an application target
SuilApp(suil-bench
    SOURCES
        src/main.cpp
        ${SUIL_PROJECT_SCC_PUB}/app.scc.cpp
    LIBRARIES Suil::HttpServer
    INSTALL   ON
    DEPENDS   suil-bench-scc
    DEFINES   SUIL_BENCH_DEV=${SUIL_BENCH_DEV})

# Needed for installation to work
SuilEndProject()