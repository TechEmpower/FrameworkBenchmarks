
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/te-benchmark-um-pq-async/include")
if(BUILD_STATIC_LIBS OR EMSCRIPTEN)
	add_library(te-benchmark-um-pq-async STATIC ${sources})
else()
	add_library(te-benchmark-um-pq-async ${sources})
endif()
set_property(TARGET te-benchmark-um-pq-async PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(te-benchmark-um-pq-async ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
