
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/te-benchmark-um-pq-async/include")
add_library(te_benchmark_um_pq_async SHARED ${sources})
set_property(TARGET te_benchmark_um_pq_async PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(te_benchmark_um_pq_async ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
