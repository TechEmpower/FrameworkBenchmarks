cmake_minimum_required(VERSION 3.2)
Project(drogon_benchmark)

set(CMAKE_CXX_STANDARD 17)

find_package(Drogon REQUIRED)
link_libraries(${DROGON_LIBRARIES})
include_directories(${DROGON_INCLUDE_DIRS})

AUX_SOURCE_DIRECTORY(./ SRC_DIR)
AUX_SOURCE_DIRECTORY(controllers CTL_SRC)
AUX_SOURCE_DIRECTORY(filters FILTER_SRC)
AUX_SOURCE_DIRECTORY(models MODEL_SRC)

FILE(GLOB SCP_LIST ${CMAKE_CURRENT_SOURCE_DIR}/views/*.csp)
foreach(cspFile ${SCP_LIST})
    message(STATUS "cspFile:" ${cspFile})
    EXEC_PROGRAM(basename ARGS "${cspFile} .csp" OUTPUT_VARIABLE classname)
    message(STATUS "view classname:" ${classname})
    add_custom_command(OUTPUT ${classname}.h ${classname}.cc
            COMMAND drogon_ctl
            ARGS create view ${cspFile}
            DEPENDS ${cspFile}
            VERBATIM )
    set(VIEWSRC ${VIEWSRC} ${classname}.cc)
endforeach()

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_executable(drogon_benchmark ${SRC_DIR} ${CTL_SRC} ${FILTER_SRC} ${VIEWSRC} ${MODEL_SRC})

add_custom_command(TARGET drogon_benchmark POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy_if_different
                   ${CMAKE_SOURCE_DIR}/*.json $<TARGET_FILE_DIR:drogon_benchmark>)
