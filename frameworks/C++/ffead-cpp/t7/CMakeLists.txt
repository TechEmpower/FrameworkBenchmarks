
file(GLOB sources
    "include/*.h"
    "src/*.cpp"
)

include_directories("${CMAKE_SOURCE_DIR}/web/t7/include")
if(BUILD_STATIC_LIBS OR EMSCRIPTEN)
	add_library(t7 STATIC ${sources})
else()
	add_library(t7 ${sources})
endif()
set_property(TARGET t7 PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(t7 ffead-modules ffead-framework ${HAVE_PQLIB} ${HAVE_CURLLIB} ${HAVE_SSLLIB} ${HAVE_MEMCACHEDLIB} ${HAVE_ODBCLIB} ${HAVE_MONGOCLIB} ${HAVE_BSONLIB} ${HAVE_ZLIB} ${HAVE_CRYPTOLIB})
