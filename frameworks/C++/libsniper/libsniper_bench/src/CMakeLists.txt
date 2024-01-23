include_directories(.)
include_directories(${SNIPER_INCLUDE_DIRS})

set(PROJECT_LIBS
        ${CMAKE_THREAD_LIBS_INIT}
        ${BOOST_LIBRARIES}
        backtrace dl
        stdc++fs.a
        tcmalloc
        )

set(PROJECT_SRC
        main.cpp
        )

add_executable(${PROJECT_NAME} ${PROJECT_SRC})
target_link_libraries(${PROJECT_NAME} ${SNIPER_LIBRARIES} ${PROJECT_LIBS})
