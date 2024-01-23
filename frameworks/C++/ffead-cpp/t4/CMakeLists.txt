
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/t4/include")
if(BUILD_STATIC_LIBS OR EMSCRIPTEN)
	add_library(t4 STATIC ${sources})
else()
	add_library(t4 ${sources})
endif()
set_property(TARGET t4 PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(t4 ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
