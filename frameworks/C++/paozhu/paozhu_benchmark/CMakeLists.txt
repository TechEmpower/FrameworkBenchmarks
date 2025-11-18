CMAKE_MINIMUM_REQUIRED(VERSION 3.20)

cmake_policy(SET CMP0048 NEW)

set(ENABLE_VCPKG OFF CACHE BOOL "choose ON to enable")
set(ENABLE_BOOST OFF CACHE BOOL "choose ON to enable")
set(ENABLE_GD OFF CACHE BOOL "choose ON to enable")
set(ENABLE_WIN_VCPKG OFF CACHE BOOL "choose ON to enable")

if(ENABLE_VCPKG)
    set(vcpkg_root $ENV{VCPKG_ROOT})
    if(NOT DEFINED CMAKE_TOOLCHAIN_FILE AND vcpkg_root)
        set(CMAKE_TOOLCHAIN_FILE "${vcpkg_root}/scripts/buildsystems/vcpkg.cmake")
    endif()
endif()

macro(auto_enable_vcpkg_feature opt feature)
    if(${opt})
        list(FIND VCPKG_MANIFEST_FEATURES ${feature} index)
        if(index EQUAL -1)
            message(STATUS "Auto append features: ${feature}")
            list(APPEND VCPKG_MANIFEST_FEATURES ${feature})
        endif()
    endif()
endmacro()

if(ENABLE_VCPKG)
  auto_enable_vcpkg_feature(ENABLE_GD "gd")
  auto_enable_vcpkg_feature(ENABLE_BOOST "boost")
endif()

PROJECT(Paozhu_web_framework)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(mode "CMAKE_BUILD_TYPE")

set(BOOST_OPEN "  ")
set(GD_OPEN " ")

if(ENABLE_BOOST STREQUAL "ON")
  message("ENABLE_BOOST")
  set(BOOST_OPEN " -DENABLE_BOOST ")
endif()

if(ENABLE_GD STREQUAL "ON")
    message("ENABLE_GD")
    set(GD_OPEN " -DENABLE_GD ")
endif()

set(sys_so_path "/usr/lib64")

if (IS_DIRECTORY "/usr/lib/x86_64-linux-gnu")
  set(sys_so_path "/usr/lib/x86_64-linux-gnu")
endif()

if (CMAKE_SYSTEM_NAME MATCHES "Windows")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_WIN32_WINNT=0x0601")
endif()

if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /utf-8 /EHsc")
endif ()

if(${mode} AND (CMAKE_BUILD_TYPE STREQUAL "Debug"))

    if(CMAKE_SYSTEM_NAME MATCHES "Windows")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DASIO_STANDALONE -DBENCHMARK -DDEBUG ${BOOST_OPEN} ${GD_OPEN}")
        if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /fsanitize=address")
        elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pthread -g")
        endif ()
    else()
        set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -Wextra")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pthread -g -fsanitize=address -DASIO_STANDALONE -DBENCHMARK -DDEBUG ${BOOST_OPEN} ${GD_OPEN} -I/usr/local/include -I/usr/include " )
    endif ()
    message("Debug mode:${CMAKE_CXX_FLAGS_DEBUG}")

elseif(${mode} AND (CMAKE_BUILD_TYPE STREQUAL "Release"))

    if (CMAKE_SYSTEM_NAME MATCHES "Windows")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DASIO_STANDALONE ${BOOST_OPEN} ${GD_OPEN} -DBENCHMARK  -O3")
        if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pthread")
        endif ()
    else ()
        set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wall -Wextra -O3")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread  -DASIO_STANDALONE ${BOOST_OPEN} ${GD_OPEN} -DBENCHMARK -I/usr/local/include -I/usr/include  " )
        file(MAKE_DIRECTORY /usr/local/etc/paozhu)
        file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/conf/ DESTINATION /usr/local/etc/paozhu/)
    endif ()
    message("Release mode:${CMAKE_CXX_FLAGS_RELEASE}")

else()
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -Wextra")
    message("Debug mode:${CMAKE_CXX_FLAGS_DEBUG}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pthread  -g -fsanitize=address  -DASIO_STANDALONE -DBENCHMARK -DDEBUG ${BOOST_OPEN} ${GD_OPEN} -I/usr/local/include -I/usr/include " )

    if(NOT CMAKE_SYSTEM_NAME MATCHES "Windows")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
    endif ()
endif()



file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/temp)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/log)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_CURRENT_SOURCE_DIR}/bin)
set(USE_STANDALONE_ASIO ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

message(STATUS "SOURCE dir ${CMAKE_CURRENT_SOURCE_DIR}")


set(CMAKE_BUILD_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/build)
set(CMAKE_CACHEFILE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/build)
set(CMAKE_CURRENT_BINARY_DIR ${CMAKE_CURRENT_SOURCE_DIR}/build)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/common)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/controller)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpcli)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/viewsrc/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/websockets/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/orm)

if (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/bin/paozhu_pre)
  file(REMOVE_RECURSE ${CMAKE_CURRENT_SOURCE_DIR}/bin/paozhu_pre)
endif()
set(PAOZHU_PRE paozhu_pre)
add_executable(${PAOZHU_PRE} ${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpcli/autopickmethod.cpp ${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpserver/src/md5.cpp)



file(GLOB_RECURSE orm_list ${CMAKE_CURRENT_SOURCE_DIR}/orm/*.cpp)
file(GLOB_RECURSE source_list ${CMAKE_CURRENT_SOURCE_DIR}/models/*.cpp)
file(GLOB_RECURSE FRAMEWORK_CPP_PATH ${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpserver/*.cpp)
file(GLOB_RECURSE common_list ${CMAKE_CURRENT_SOURCE_DIR}/common/*.cpp)
file(GLOB_RECURSE controller_list ${CMAKE_CURRENT_SOURCE_DIR}/controller/src/*.cpp)
file(GLOB_RECURSE viewsrc_list ${CMAKE_CURRENT_SOURCE_DIR}/viewsrc/view/*.cpp)
file(GLOB_RECURSE reflect_list ${CMAKE_CURRENT_SOURCE_DIR}/libs/*.cpp)
file(GLOB_RECURSE src_list ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp)

foreach(cppfile IN LISTS controller_list)
  string(REGEX REPLACE "${CMAKE_CURRENT_SOURCE_DIR}/controller/src/" "" cppfilename ${cppfile})
  string(FIND ${cppfilename} "/" strpos)
  if(${strpos} GREATER_EQUAL "0" )
      string(REGEX MATCHALL "([A-Za-z0-9._-]+)/" npaths ${cppfilename} )
      set(fullpaths "")
      string(APPEND fullpaths "${CMAKE_CURRENT_SOURCE_DIR}/controller/include/")
      foreach(onepathname ${npaths})
            string(REPLACE "/" "" toucpath ${onepathname})
            string(APPEND fullpaths ${toucpath})
            if (IS_DIRECTORY "${fullpaths}") 
            else()
                  message("mkdir ${fullpaths}") 
                  file(MAKE_DIRECTORY "${fullpaths}")
            endif()
            string(APPEND fullpaths "/")
      endforeach()
  endif()
  string(REGEX REPLACE ".cpp" "" cppbasename ${cppfilename})
#  message(${cppbasename})
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/controller/include/${cppbasename}.h")
  else()
      file(TOUCH "${CMAKE_CURRENT_SOURCE_DIR}/controller/include/${cppbasename}.h")
  endif()
endforeach()

function(include_sub_directories_recursively root_dir)
    if (IS_DIRECTORY ${root_dir})               # 当前路径是一个目录吗，是的话就加入到包含目录
        #        if (${root_dir} MATCHES "include")
        message("include dir: " ${root_dir})
        include_directories(${root_dir})
        #        endif()
    endif()

    file(GLOB ALL_SUB RELATIVE ${root_dir} ${root_dir}/*) # 获得当前目录下的所有文件，让如ALL_SUB列表中

    foreach(sub ${ALL_SUB})
        if (IS_DIRECTORY ${root_dir}/${sub})
            include_sub_directories_recursively(${root_dir}/${sub}) # 对子目录递归调用，包含
        endif()
    endforeach()
endfunction()

include_sub_directories_recursively(${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpserver)
include_sub_directories_recursively(${CMAKE_CURRENT_SOURCE_DIR}/orm)
include_sub_directories_recursively(${CMAKE_CURRENT_SOURCE_DIR}/models)
include_sub_directories_recursively(${CMAKE_CURRENT_SOURCE_DIR}/controller/include)
include_sub_directories_recursively(${CMAKE_CURRENT_SOURCE_DIR}/libs)
include_sub_directories_recursively(${CMAKE_CURRENT_SOURCE_DIR}/include)
include_sub_directories_recursively(${CMAKE_CURRENT_SOURCE_DIR}/common)

add_executable(paozhu_cli ${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpcli/http_cli.cpp ${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpserver/src/mysql_conn.cpp ${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpserver/src/mysql_conn_pool.cpp ${CMAKE_CURRENT_SOURCE_DIR}/vendor/httpserver/src/clientdatacache.cpp)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin)


if(${mode} AND (CMAKE_BUILD_TYPE STREQUAL "Release"))
  add_executable(paozhu ${CMAKE_CURRENT_SOURCE_DIR}/startup/main_docker.cpp ${common_list} ${viewsrc_list} ${FRAMEWORK_CPP_PATH} ${orm_list} ${reflect_list} ${src_list} ${source_list} ${controller_list})
else()
  add_executable(paozhu ${CMAKE_CURRENT_SOURCE_DIR}/startup/main_dev.cpp ${common_list} ${viewsrc_list} ${FRAMEWORK_CPP_PATH} ${orm_list} ${reflect_list} ${src_list} ${source_list} ${controller_list})
endif()

if (ENABLE_WIN_VCPKG STREQUAL "ON")
else()
add_custom_command(
    TARGET paozhu_pre POST_BUILD
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
    COMMAND echo "-- controls method --"
    COMMAND ${PAOZHU_PRE} ${CMAKE_CURRENT_SOURCE_DIR}/
    )
endif()

if (ENABLE_VCPKG)
    add_compile_definitions(ENABLE_VCPKG)
    find_package(asio CONFIG REQUIRED)
    target_link_libraries(paozhu asio::asio)

    if (ENABLE_BOOST)
        add_compile_definitions(ENABLE_BOOST)
        set(Boost_NO_WARN_NEW_VERSIONS ON)
        find_package(Boost REQUIRED COMPONENTS filesystem coroutine)
        target_link_libraries(paozhu Boost::boost Boost::filesystem Boost::coroutine)
    endif ()

    find_package(OpenSSL REQUIRED)
    target_link_libraries(paozhu OpenSSL::Crypto OpenSSL::SSL)

    find_package(ZLIB REQUIRED)
    target_link_libraries(paozhu ZLIB::ZLIB)

    # find_package(libmysql REQUIRED)
    # find_package(unofficial-libmariadb CONFIG REQUIRED)
    # find_path(MYSQL_ROOT_DIR mysql)
    # target_link_libraries(paozhu ${MYSQL_LIBRARIES})
    # target_link_libraries(paozhu_cli ${MYSQL_LIBRARIES})
    # target_link_libraries(paozhu unofficial::libmariadb)
    # target_link_libraries(paozhu_cli unofficial::libmariadb)
    # target_include_directories(paozhu PUBLIC ${MYSQL_ROOT_DIR}/mysql)
    # target_include_directories(paozhu_cli PUBLIC ${MYSQL_ROOT_DIR}/mysql)

    target_link_libraries(paozhu_cli asio::asio)
    target_link_libraries(paozhu_cli OpenSSL::Crypto OpenSSL::SSL)

    if (ENABLE_GD STREQUAL "ON")
        find_package(PkgConfig)
        pkg_check_modules(LIBGD REQUIRED IMPORTED_TARGET gdlib)
        target_link_libraries(paozhu PkgConfig::LIBGD)

        find_path(QRENCODE_INCLUDE_DIR NAMES qrencode.h)
        find_library(QRENCODE_LIBRARY_RELEASE qrencode)
        # find_library(QRENCODE_LIBRARY_DEBUG qrencoded)
        # set(QRENCODE_LIBRARIES optimized ${QRENCODE_LIBRARY_RELEASE} debug ${QRENCODE_LIBRARY_DEBUG})
        target_include_directories(paozhu PRIVATE ${QRENCODE_INCLUDE_DIR})
        target_link_libraries(paozhu ${QRENCODE_LIBRARY_RELEASE})
        # MESSAGE(STATUS ${QRENCODE_LIBRARY_RELEASE})

        # warning: Fixed an issue where ports/libqrencode would not automatically
        #          copy dll files to the bin directory
        if (CMAKE_SYSTEM_NAME MATCHES "Windows")
            set(PATH_TO_QRENCODE_DLL "")
            if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
                set(PATH_TO_QRENCODE_DLL "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/bin/qrencode.dll")
            elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
                set(PATH_TO_QRENCODE_DLL "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libqrencode.dll")
            endif ()
            message(STATUS "Selected libqrencode.dll: ${PATH_TO_QRENCODE_DLL}")

            add_custom_command(
                    TARGET paozhu POST_BUILD
                    COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    ${PATH_TO_QRENCODE_DLL} ${CMAKE_CURRENT_SOURCE_DIR}/bin/
            )
        endif ()

        find_package(PNG REQUIRED)
        target_link_libraries(paozhu PNG::PNG)

        find_package(Freetype REQUIRED)
        target_link_libraries(paozhu Freetype::Freetype)
    endif ()

    find_package(unofficial-brotli CONFIG REQUIRED)
    target_link_libraries(paozhu unofficial::brotli::brotlidec unofficial::brotli::brotlienc)
else ()

if(USE_STANDALONE_ASIO)
if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
set(ASIO_PATH "/usr/local/opt/asio/include" "/usr/local/include" "/opt/homebrew/opt/asio" ${CMAKE_CURRENT_SOURCE_DIR}/asio)
else()
set(ASIO_PATH ${CMAKE_CURRENT_SOURCE_DIR}/asio "/usr/include")
endif()

    target_compile_definitions(paozhu INTERFACE ASIO_STANDALONE)
    find_path(ASIO_PATH asio.hpp)
    message(state " Standalone Asio found: " ${ASIO_PATH})
    if(NOT ASIO_PATH)
        message(FATAL_ERROR "Standalone Asio not found")
    else()
        target_include_directories(paozhu INTERFACE ${ASIO_PATH})
    endif()

    include_directories(${ASIO_PATH})

endif()


if(ENABLE_BOOST STREQUAL "ON")
message("---ENABLE_BOOST-----")
find_package(Boost REQUIRED
             COMPONENTS system filesystem)
if(Boost_FOUND)
    add_compile_definitions(ENABLE_BOOST)
    include_directories("${Boost_INCLUDE_DIRS}/boost")

    MESSAGE( STATUS "Boost_INCLUDE_DIRS = ${Boost_INCLUDE_DIRS}")
    MESSAGE( STATUS "Boost_LIBRARIES = ${Boost_LIBRARY_DIRS}")
    MESSAGE( STATUS "Boost_LIB_VERSION = ${Boost_LIB_VERSION}")
    link_directories(${Boost_LIBRARY_DIRS})
    target_link_libraries (paozhu ${Boost_LIBRARIES})
endif()

endif()


find_package(OpenSSL REQUIRED)

if(OPENSSL_FOUND)

  message(STATUS "OPENSSL_VERSION = ${OPENSSL_VERSION}")
  message(STATUS "OPENSSL_SSL_LIBRARY = ${OPENSSL_SSL_LIBRARY}")
  message(STATUS "OPENSSL_CRYPTO_LIBRARY = ${OPENSSL_CRYPTO_LIBRARY}")
  message(STATUS "OPENSSL_INCLUDE_DIR = ${OPENSSL_INCLUDE_DIR}")
  INCLUDE_DIRECTORIES("${OPENSSL_INCLUDE_DIR}")
  target_link_libraries (paozhu ${OPENSSL_SSL_LIBRARY})
  target_link_libraries (paozhu ${OPENSSL_CRYPTO_LIBRARY})
  
  target_link_libraries (paozhu_cli ${OPENSSL_SSL_LIBRARY})
  target_link_libraries (paozhu_cli ${OPENSSL_CRYPTO_LIBRARY})
endif()


find_package(ZLIB REQUIRED)
if(ZLIB_FOUND)
  message(STATUS "Zlib Found! ${ZLIB_LIBRARIES}")
  include_directories(${ZLIB_INCLUDE_DIR})
  set(zlib_library ${ZLIB_LIBRARIES})
  target_link_libraries(paozhu z)
endif()


#if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
#  if (IS_DIRECTORY "/usr/local/mysql/include")
# MESSAGE( STATUS "/usr/local/mysql")
#     set(MYSQL_INCLUDE_DIR "/usr/local/mysql/include")
#     include_sub_directories_recursively(/usr/local/mysql/include)
# endif()
#
# if (IS_DIRECTORY "/usr/local/opt/mysql-client")
#       MESSAGE( STATUS "/usr/local/opt/mysql-client")
#     set(MYSQL_INCLUDE_DIR "/usr/local/opt/mysql-client/include")
#     include_sub_directories_recursively(/usr/local/opt/mysql-client/include)
# endif()
# 
# if (IS_DIRECTORY "/opt/homebrew/opt/mysql")
#       MESSAGE( STATUS "/opt/homebrew/opt/mysql")
#     set(MYSQL_INCLUDE_DIR "/opt/homebrew/opt/mysql/include")
#     include_sub_directories_recursively(/opt/homebrew/opt/mysql/include)
# endif()
#   
#   
#   if (IS_DIRECTORY "/opt/homebrew/opt/mysql-client")
#         MESSAGE( STATUS "/opt/homebrew/opt/mysql-client")
#       set(MYSQL_INCLUDE_DIR "/opt/homebrew/opt/mysql-client/include")
#       include_sub_directories_recursively(/opt/homebrew/opt/mysql-client/include)
#   endif()
#   MESSAGE( STATUS "MYSQL_ROOT_DIR = ${MYSQL_ROOT_DIR} ")
#else()  
#  find_path(MYSQL_ROOT_DIR mysql)
#endif()
# 
# 
# FIND_PATH(MYSQL_INCLUDE_DIR NAMES mysql.h
#   PATHS /usr/local/include/mysql /usr/include/mysql /opt/homebrew/opt/mysql/include /usr/local/opt/mysql-client/include /opt/homebrew/opt/mysql-client/include
# )
# 
# MESSAGE( STATUS "MYSQL_ROOT_DIR = ${MYSQL_ROOT_DIR} ")
# find_package_handle_standard_args(mysql REQUIRED_VARS MYSQL_ROOT_DIR)
# 
# 
# if(NOT MYSQL_INCLUDE_DIR)
#   message(STATUS "Could not find \"mysql.h\" from searching ")
# endif()
# 
# SET(MYSQL_NAMES mysqlclient)
# FIND_LIBRARY(MYSQL_LIBRARY
#   NAMES ${MYSQL_NAMES}
#   PATHS /usr/lib /usr/local/lib /usr/local/mysql/lib /usr/local/opt/mysql/lib /opt/homebrew/opt/mysql/lib /opt/homebrew/opt/mysql-client/lib
#   PATH_SUFFIXES mysql
# )
# 
# IF (MYSQL_INCLUDE_DIR AND MYSQL_LIBRARY)
#   SET(MYSQL_FOUND TRUE)
#   SET( MYSQL_LIBRARIES ${MYSQL_LIBRARY} )
# ELSE (MYSQL_INCLUDE_DIR AND MYSQL_LIBRARY)
#   SET(MYSQL_FOUND FALSE)
#   SET( MYSQL_LIBRARIES )
# ENDIF (MYSQL_INCLUDE_DIR AND MYSQL_LIBRARY)
# 
# IF (MYSQL_FOUND)
#   IF (NOT MYSQL_FIND_QUIETLY)
#     MESSAGE(STATUS "Found MySQL: ${MYSQL_LIBRARY}")
#   ENDIF (NOT MYSQL_FIND_QUIETLY)
# ELSE (MYSQL_FOUND)
#   IF (MYSQL_FIND_REQUIRED)
#     MESSAGE(STATUS "Looked for MySQL libraries named ${MYSQL_NAMES}.")
#     MESSAGE(FATAL_ERROR "Could NOT find MySQL library")
#   ENDIF (MYSQL_FIND_REQUIRED)
# ENDIF (MYSQL_FOUND)
# 
# target_include_directories(paozhu PUBLIC ${MYSQL_INCLUDE_DIR})
# target_link_libraries(paozhu ${MYSQL_LIBRARY})
# 
# target_include_directories(paozhu_cli PUBLIC ${MYSQL_INCLUDE_DIR})
# target_link_libraries(paozhu_cli ${MYSQL_LIBRARY})



if(ENABLE_GD STREQUAL "ON")
message("---ENABLE_GD-----")

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  
  if (IS_DIRECTORY "/usr/local/opt/gd/lib")
        MESSAGE( STATUS "/usr/local/opt/gd/lib")
      set(GD_ROOT_DIR "/usr/local/opt/gd/lib")
      include_sub_directories_recursively(/usr/local/opt/gd/include)
  endif()
  if (IS_DIRECTORY "/opt/homebrew/opt/gd/lib")
        MESSAGE( STATUS "/opt/homebrew/opt/gd/lib")
      set(GD_ROOT_DIR "/opt/homebrew/opt/gd/lib")
      include_sub_directories_recursively(/opt/homebrew/opt/gd/include)
  endif()
else()
  set(GD_ROOT_DIR "${sys_so_path}")
endif()

set(find_gdname gd)

find_library(GD_LIB_DIR
  NAMES ${find_gdname}
  PATHS "${GD_ROOT_DIR}"
  NO_DEFAULT_PATH
)

if(NOT GD_LIB_DIR)
message(FATAL_ERROR
"GD Graphics Library NOT FOUND! please install . "
)
endif()

message(STATUS "GD Graphics Library  at: ${GD_LIB_DIR}")

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")

  if (IS_DIRECTORY "/usr/local/opt/qrencode/lib")
        MESSAGE( STATUS "/usr/local/opt/qrencode/lib")
      set(QR_ROOT_DIR "/usr/local/opt/qrencode/lib")
      include_sub_directories_recursively(/usr/local/opt/qrencode/include)
  endif()
  if (IS_DIRECTORY "/opt/homebrew/opt/qrencode/lib")
        MESSAGE( STATUS "/opt/homebrew/opt/qrencode/lib")
      set(QR_ROOT_DIR "/opt/homebrew/opt/qrencode/lib")
      include_sub_directories_recursively(/opt/homebrew/opt/qrencode/include)
  endif()
  
else()
set(QR_ROOT_DIR "${sys_so_path}")
endif()

set(find_qrname qrencode)

find_library(QR_LIB_DIR
  NAMES ${find_qrname}
  PATHS "${QR_ROOT_DIR}"
  NO_DEFAULT_PATH
)

if(NOT QR_LIB_DIR)
message(FATAL_ERROR
"qrencode Library  NOT FOUND! please install . "
)
endif()

message(STATUS "qrencode at: ${QR_LIB_DIR}")

INCLUDE_DIRECTORIES("${GD_ROOT_DIR}/include")
INCLUDE_DIRECTORIES("${QR_ROOT_DIR}/include")
link_directories("${QR_ROOT_DIR}/lib")
link_directories("${GD_ROOT_DIR}/lib")
target_link_libraries(paozhu ${GD_LIB_DIR})
target_link_libraries(paozhu ${QR_LIB_DIR})

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")

  if (IS_DIRECTORY "/usr/local/opt/libpng/lib")
        MESSAGE( STATUS "/usr/local/opt/libpng/lib")
      set(PNG_ROOT_DIR "/usr/local/opt/libpng/lib")
      include_sub_directories_recursively(/usr/local/opt/libpng/include)
  endif()
  if (IS_DIRECTORY "/opt/homebrew/opt/libpng/lib")
        MESSAGE( STATUS "/opt/homebrew/opt/libpng/lib")
      set(PNG_ROOT_DIR "/opt/homebrew/opt/libpng/lib")
      include_sub_directories_recursively(/opt/homebrew/opt/libpng/include)
  endif()
  
else()
set(PNG_ROOT_DIR "${sys_so_path}")
endif()

find_library(PNG_LIB_DIR
  NAMES png
  PATHS "${PNG_ROOT_DIR}"
  NO_DEFAULT_PATH
)
target_link_libraries(paozhu ${PNG_LIB_DIR})

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  
    if (IS_DIRECTORY "/usr/local/opt/freetype/lib")
        MESSAGE( STATUS "/usr/local/opt/freetype/lib")
      set(FREETYPE_ROOT_DIR "/usr/local/opt/freetype/lib")
      include_sub_directories_recursively(/usr/local/opt/freetype/include)
  endif()
  if (IS_DIRECTORY "/opt/homebrew/opt/freetype/lib")
        MESSAGE( STATUS "/opt/homebrew/opt/freetype/lib")
      set(FREETYPE_ROOT_DIR "/opt/homebrew/opt/freetype/lib")
      include_sub_directories_recursively(/opt/homebrew/opt/freetype/include)
  endif()
  
else()
  set(FREETYPE_ROOT_DIR "${sys_so_path}")
endif()

find_library(FREETYPE_LIB_DIR
  NAMES freetype
  PATHS "${FREETYPE_ROOT_DIR}"
  NO_DEFAULT_PATH
)
target_link_libraries(paozhu ${FREETYPE_LIB_DIR})
#end ENABLE_GD
endif()

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  if (IS_DIRECTORY "/usr/local/opt/brotli/lib")
      set(BROTLI_ROOT_DIR "/usr/local/opt/brotli/lib")
      INCLUDE_DIRECTORIES("/usr/local/opt/brotli/include")
  endif()
#  set(BROTLI_ROOT_DIR "/usr/local/opt/brotli/lib")
  if (IS_DIRECTORY "/opt/homebrew/opt/brotli/lib")
      set(BROTLI_ROOT_DIR "/opt/homebrew/opt/brotli/lib")
      INCLUDE_DIRECTORIES("/opt/homebrew/opt/brotli/include")
  endif()
else()
  set(BROTLI_ROOT_DIR "${sys_so_path}")
endif()



#find_package(Brotli COMPONENTS encoder decoder common REQUIRED)

message(STATUS "Brotli at: ${BROTLI_ROOT_DIR}")

set(find_brname brotlienc)
set(find_brdename brotlidec)
find_library(BR_LIB_DIR
  NAMES ${find_brname}
  PATHS "${BROTLI_ROOT_DIR}"
  NO_DEFAULT_PATH
)
find_library(BRDEC_LIB_DIR
  NAMES ${find_brdename}
  PATHS "${BROTLI_ROOT_DIR}"
  NO_DEFAULT_PATH
)
INCLUDE_DIRECTORIES("${BROTLI_ROOT_DIR}/include")
link_directories("${BROTLI_ROOT_DIR}/lib")

if(NOT BR_LIB_DIR)
message(FATAL_ERROR
"Brotli Library  NOT FOUND! please install . "
)
endif()

message(STATUS "Brotli at: ${BR_LIB_DIR}")
target_link_libraries(paozhu ${BR_LIB_DIR})


if(NOT BRDEC_LIB_DIR)
message(FATAL_ERROR
"Brotli Library  NOT FOUND! please install . "
)
endif()

message(STATUS "Brotli at: ${BRDEC_LIB_DIR}")
target_link_libraries(paozhu ${BRDEC_LIB_DIR})


message("Compile framework mode")

target_link_libraries(paozhu  m  dl)

endif ()

if (CMAKE_SYSTEM_NAME MATCHES "Windows")
   target_link_libraries(paozhu ws2_32)
   target_link_libraries(paozhu_cli ws2_32)
endif ()
