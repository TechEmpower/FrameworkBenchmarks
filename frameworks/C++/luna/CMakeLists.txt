cmake_minimum_required(VERSION 2.8)
project(lunabench)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")

find_package( Threads )

include(conanbuildinfo.cmake)
conan_basic_setup()

set(DEFAULT_SOURCE_FILES default.cpp common.h)
add_executable(lunabench_default ${DEFAULT_SOURCE_FILES})
target_link_libraries(lunabench_default ${CONAN_LIBS} ${CMAKE_THREAD_LIBS_INIT})

set(EPOLL_SOURCE_FILES epoll.cpp common.h)
add_executable(lunabench_epoll ${EPOLL_SOURCE_FILES})
target_link_libraries(lunabench_epoll ${CONAN_LIBS} ${CMAKE_THREAD_LIBS_INIT})

set(THREAD_SOURCE_FILES thread.cpp common.h)
add_executable(lunabench_thread ${THREAD_SOURCE_FILES})
target_link_libraries(lunabench_thread ${CONAN_LIBS} ${CMAKE_THREAD_LIBS_INIT})