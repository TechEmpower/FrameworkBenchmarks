set(COMMON_FLAGS "\
    -march=native \
    -pipe \
    -m64 \
    -msse \
    -msse2 \
    -msse3 \
    -mssse3 \
    -msse4 \
    -msse4.1 \
    -msse4.2 \
    -mavx \
    -Wall \
    -Wextra \
    -Werror \
    -Wpedantic \
    -Wformat-security \
    -fno-builtin-malloc \
    -fno-builtin-calloc \
    -fno-builtin-realloc \
    -fno-builtin-free \
    -Wno-unused-parameter \
    -Wno-unused-but-set-variable \
    ")


set(COMMON_FLAGS_DEBUG "\
    -Og \
    -g \
    -rdynamic \
    ")

set(COMMON_FLAGS_RELWITHDEBINFO "\
    -g \
    -DNDEBUG \
    -Ofast \
    -rdynamic \
    -funroll-loops \
    -fomit-frame-pointer \
    -Wno-misleading-indentation \
    ")


set(COMMON_FLAGS_RELEASE "\
    -DNDEBUG \
    -Ofast \
    -s \
    -funroll-loops \
    -fomit-frame-pointer \
    -Wno-misleading-indentation \
    ")

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    set(COMMON_FLAGS "\
        ${COMMON_FLAGS} \
         -pthread \
        ")

    set(COMMON_FLAGS_DEBUG "\
        ${COMMON_FLAGS_DEBUG} \
        -fsanitize=address \
        -fsanitize=undefined \
        -fsanitize=leak \
        -fstack-protector \
        -fuse-ld=gold \
        ")

endif ()

if (UNIX AND !APPLE)
    message("USE LTO")
    include(CheckIPOSupported)
    check_ipo_supported(RESULT supported OUTPUT error)
    set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
endif ()

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${COMMON_FLAGS}")
set(CMAKE_C_FLAGS_DEBUG "${COMMON_FLAGS_DEBUG}")
set(CMAKE_C_FLAGS_RELEASE "${COMMON_FLAGS_RELEASE}")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${COMMON_FLAGS} -Wno-class-memaccess")
set(CMAKE_CXX_FLAGS_DEBUG "${COMMON_FLAGS_DEBUG}")
set(CMAKE_CXX_FLAGS_RELEASE "${COMMON_FLAGS_RELEASE}")

set(CMAKE_C_FLAGS_RELWITHDEBINFO "${COMMON_FLAGS_RELWITHDEBINFO}")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${COMMON_FLAGS_RELWITHDEBINFO}")
