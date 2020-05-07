set(SNIPER_INCLUDE_DIRS "" CACHE INTERNAL "sniper_include_dirs")
set(SNIPER_LIBRARIES "" CACHE INTERNAL "sniper_libraries")

# add all subfolders
file(GLOB children RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/*)
foreach(child ${children})
    if(IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${child})
        set(SNIPER_INCLUDE_DIRS ${SNIPER_INCLUDE_DIRS} "${CMAKE_CURRENT_SOURCE_DIR}/${child}" CACHE INTERNAL "sniper_include_dirs")
    endif()
endforeach()

include_directories(BEFORE ${SNIPER_INCLUDE_DIRS})

file(GLOB children RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/*)
foreach(child ${children})
    if(IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${child})
        add_subdirectory(${child})
    endif()
endforeach()
