
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/t5/include")
if(BUILD_STATIC_LIBS OR EMSCRIPTEN)
	add_library(t5 STATIC ${sources})
else()
	add_library(t5 ${sources})
endif()
set_property(TARGET t5 PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(t5 ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
