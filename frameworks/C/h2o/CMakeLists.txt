cmake_minimum_required(VERSION 3.18.0)
project(h2o-app)
find_library(BPF_LIB bpf REQUIRED)
find_library(CRYPTO_LIB crypto REQUIRED)
find_library(H2O_LIB h2o-evloop REQUIRED)
find_library(MUSTACHE_C_LIB mustache_c REQUIRED)
find_library(NUMA_LIB numa REQUIRED)
find_library(PQ_LIB pq REQUIRED)
find_library(SSL_LIB ssl REQUIRED)
find_library(YAJL_LIB yajl REQUIRED)
find_library(Z_LIB z REQUIRED)
find_path(ASM_INCLUDE asm/types.h REQUIRED)
find_path(BPF_INCLUDE bpf/libbpf.h REQUIRED)
find_path(H2O_INCLUDE h2o.h REQUIRED)
find_path(MUSTACHE_C_INCLUDE mustache.h REQUIRED)
find_path(NUMA_INCLUDE numaif.h REQUIRED)
find_path(OPENSSL_INCLUDE openssl/ssl.h REQUIRED)
find_path(PQ_INCLUDE postgresql/libpq-fe.h REQUIRED)
find_path(YAJL_INCLUDE yajl/yajl_gen.h REQUIRED)
find_program(BPFTOOL_BIN bpftool REQUIRED)
find_program(CLANG_BIN clang REQUIRED)
include_directories(src ${CMAKE_BINARY_DIR}/generated-headers ${BPF_INCLUDE} ${H2O_INCLUDE})
include_directories(${MUSTACHE_C_INCLUDE} ${NUMA_INCLUDE} ${OPENSSL_INCLUDE} ${PQ_INCLUDE})
include_directories(${YAJL_INCLUDE})
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
add_compile_definitions(H2O_USE_LIBUV=0)
set(COMMON_OPTIONS -flto=auto -pthread)
set(WARNING_OPTIONS -pedantic -Wall -Wextra)
add_compile_options(${COMMON_OPTIONS} ${WARNING_OPTIONS})
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -D_FORTIFY_SOURCE=2")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3")
set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -O3")
add_custom_command(
  OUTPUT ${CMAKE_BINARY_DIR}/generated-headers/socket_load_balancer.h
  COMMAND ${CLANG_BIN}
            -c
            -DNDEBUG
            -g
            -I ${ASM_INCLUDE}
            -I ${BPF_INCLUDE}
            -mcpu=v3
            -o ${CMAKE_BINARY_DIR}/socket_load_balancer.o
            -O3
            -std=gnu11
            -target bpf
            ${WARNING_OPTIONS}
            ${CMAKE_CURRENT_SOURCE_DIR}/src/bpf/socket_load_balancer.c
  COMMAND ${BPFTOOL_BIN} gen skeleton ${CMAKE_BINARY_DIR}/socket_load_balancer.o >
            ${CMAKE_BINARY_DIR}/generated-headers/socket_load_balancer.h
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/bpf/socket_load_balancer.c
  VERBATIM)
add_custom_target(
  generated_headers
  DEPENDS ${CMAKE_BINARY_DIR}/generated-headers/socket_load_balancer.h)
file(GLOB_RECURSE HANDLER_SOURCES "src/handlers/*.c")
file(GLOB SOURCES "src/*.c")
add_executable(${PROJECT_NAME} ${HANDLER_SOURCES} ${SOURCES})
add_dependencies(${PROJECT_NAME} generated_headers)
target_link_libraries(${PROJECT_NAME} ${COMMON_OPTIONS})
target_link_libraries(${PROJECT_NAME} ${BPF_LIB} ${H2O_LIB} m ${MUSTACHE_C_LIB} ${NUMA_LIB})
target_link_libraries(${PROJECT_NAME} ${PQ_LIB} ${SSL_LIB} ${CRYPTO_LIB} ${YAJL_LIB} ${Z_LIB})
install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin)
file(GLOB TEMPLATES "template/*")
install(FILES ${TEMPLATES} DESTINATION share/${PROJECT_NAME}/template)
