
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/t3/include")
if(BUILD_STATIC_LIBS OR EMSCRIPTEN)
	add_library(t3 STATIC ${sources})
else()
	add_library(t3 ${sources})
endif()
set_property(TARGET t3 PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(t3 ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
