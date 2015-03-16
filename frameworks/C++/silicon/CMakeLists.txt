cmake_minimum_required(VERSION 2.8)

project(silicon)

include_directories($ENV{IROOT}/include
                    $ENV{HOME}/projects/silicon2
                    $ENV{HOME}/projects/iod)

add_definitions(-std=c++14  -ftemplate-depth=512 -DNDEBUG -O3)

add_executable(silicon_app main.cc)
target_link_libraries(silicon_app microhttpd mysqlclient)
