
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/te-benchmark-um-mgr/include")
if(BUILD_STATIC_LIBS OR EMSCRIPTEN)
	add_library(te-benchmark-um-mgr STATIC ${sources})
else()
	add_library(te-benchmark-um-mgr ${sources})
endif()
target_link_libraries(te-benchmark-um-mgr ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
