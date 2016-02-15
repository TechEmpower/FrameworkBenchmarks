cmake_minimum_required(VERSION 2.8)

project(silicon)

include_directories($ENV{IROOT}/include $ENV{MICROHTTPD_HOME}/include)

link_directories($ENV{IROOT}/lib $ENV{MICROHTTPD_HOME}/lib)

add_definitions(-std=c++14 -ftemplate-depth=1024 -DNDEBUG -O3)

add_executable(silicon_tpc_mysql techempower_microhttpd.cc)
target_link_libraries(silicon_tpc_mysql microhttpd mysqlclient)

add_executable(silicon_epoll_mysql techempower_microhttpd.cc)
set_target_properties(silicon_epoll_mysql PROPERTIES COMPILE_FLAGS "-DTFB_USE_EPOLL")
target_link_libraries(silicon_epoll_mysql microhttpd mysqlclient)

add_executable(silicon_lwan_mysql techempower_lwan.cc)
target_link_libraries(silicon_lwan_mysql mysqlclient lwan-common curl z pthread dl)
