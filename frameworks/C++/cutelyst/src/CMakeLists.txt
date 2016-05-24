file(GLOB_RECURSE cutelyst_benchmarks_SRCS *.cpp *.h)

set(cutelyst_benchmarks_SRCS
    ${cutelyst_benchmarks_SRCS}
    ${TEMPLATES_SRC}
)

# Create the application
add_library(cutelyst_benchmarks SHARED ${cutelyst_benchmarks_SRCS})

# Link to Cutelyst
target_link_libraries(cutelyst_benchmarks
    Cutelyst::Core
    Cutelyst::Utils::Sql
    Qt5::Core
    Qt5::Network
    Qt5::Sql
)

