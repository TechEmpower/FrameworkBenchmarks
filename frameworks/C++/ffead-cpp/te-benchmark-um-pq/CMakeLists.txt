
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/te-benchmark-um-pq/include")
if(BUILD_STATIC_LIBS OR EMSCRIPTEN)
	add_library(te-benchmark-um-pq STATIC ${sources})
else()
	add_library(te-benchmark-um-pq ${sources})
endif()
set_property(TARGET te-benchmark-um-pq PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(te-benchmark-um-pq ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
