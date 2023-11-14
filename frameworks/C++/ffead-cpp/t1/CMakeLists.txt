
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/t1/include")
if(BUILD_STATIC_LIBS OR EMSCRIPTEN)
	add_library(t1 STATIC ${sources})
else()
	add_library(t1 ${sources})
endif()
target_link_libraries(t1 ffead-modules ffead-framework ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
