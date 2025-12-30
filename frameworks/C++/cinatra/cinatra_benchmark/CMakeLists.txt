cmake_minimum_required(VERSION 3.0)
set(project_name cinatra_example)
project(${project_name})
include_directories($ENV{CINATRA_HOME})
if (MSVC)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++latest")
else ()
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -O3 -pthread -std=c++20")
endif ()

add_executable(cinatra_example main.cpp)
target_compile_definitions(cinatra_example PRIVATE ASYNC_SIMPLE_HAS_NOT_AIO)

install(TARGETS ${project_name} DESTINATION include)
