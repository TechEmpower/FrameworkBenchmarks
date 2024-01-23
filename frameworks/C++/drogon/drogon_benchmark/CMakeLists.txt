cmake_minimum_required(VERSION 3.5)
project(drogon_benchmark)

set(CMAKE_CXX_STANDARD 17)
set(BENCHMARK_SOURCES
    main.cc
    controllers/DbCtrl.cc
    controllers/DbCtrlRaw.cc
    controllers/FortuneCtrl.cc
    controllers/FortuneCtrlRaw.cc
    controllers/QueriesCtrl.cc
    controllers/QueriesCtrlRaw.cc
    controllers/UpdatesCtrl.cc
    controllers/UpdatesCtrlRaw.cc
    models/Fortune.cc
    models/World.cc
    plugins/SyncPlugin.cc)

file(GLOB SCP_LIST ${CMAKE_CURRENT_SOURCE_DIR}/views/*.csp)
foreach(cspFile ${SCP_LIST})
  message(STATUS "cspFile:" ${cspFile})
  exec_program(basename
               ARGS
               "${cspFile} .csp"
               OUTPUT_VARIABLE
               classname)
  message(STATUS "view classname:" ${classname})
  add_custom_command(OUTPUT ${classname}.h ${classname}.cc
                     COMMAND drogon_ctl
                             ARGS
                             create
                             view
                             ${cspFile}
                     DEPENDS ${cspFile}
                     VERBATIM)
  set(BENCHMARK_SOURCES ${BENCHMARK_SOURCES} ${classname}.cc)
endforeach()

add_executable(${PROJECT_NAME} ${BENCHMARK_SOURCES})
find_package(Drogon REQUIRED)
find_package(mimalloc 1.0 REQUIRED)
target_link_libraries(${PROJECT_NAME} PRIVATE Drogon::Drogon mimalloc-static)

target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

add_custom_command(TARGET drogon_benchmark POST_BUILD
                   COMMAND ${CMAKE_COMMAND}
                           -E
                           copy_if_different
                           ${CMAKE_SOURCE_DIR}/*.json
                           $<TARGET_FILE_DIR:drogon_benchmark>)
