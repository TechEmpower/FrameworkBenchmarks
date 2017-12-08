cmake_minimum_required(VERSION 2.8.9)

# IncludeOS install location
if (NOT DEFINED ENV{INCLUDEOS_PREFIX})
  set(ENV{INCLUDEOS_PREFIX} /usr/local)
endif()
include($ENV{INCLUDEOS_PREFIX}/includeos/pre.service.cmake)

# Name of your project
project (mana_simple)

# Human-readable name of your service
set(SERVICE_NAME "Mana Simple Example")

# Name of your service binary
set(BINARY       "mana_simple")

# Source files to be linked with OS library parts to form bootable image
set(SOURCES
  service.cpp
  )


# DRIVERS / PLUGINS:

if ("$ENV{PLATFORM}" STREQUAL "x86_solo5")
  set(DRIVERS
      solo5net
    )
else()
  set(DRIVERS
      virtionet
    )
endif()


# THIRD PARTY LIBRARIES:

set(LIBRARIES
  "libmana.a"
  )


# include service build script
include($ENV{INCLUDEOS_PREFIX}/includeos/post.service.cmake)
