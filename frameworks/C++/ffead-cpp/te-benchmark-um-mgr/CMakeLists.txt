
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/te-benchmark-um-mgr/include")
add_library(te_benchmark_um_mgr SHARED ${sources})
set_property(TARGET te_benchmark_um_mgr PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(te_benchmark_um_mgr ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
