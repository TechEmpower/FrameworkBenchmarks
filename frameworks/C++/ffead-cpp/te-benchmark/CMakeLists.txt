
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/te-benchmark/include")
add_library(te_benchmark SHARED ${sources})
set_property(TARGET te_benchmark PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(te_benchmark ffead-modules ffead-framework ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
