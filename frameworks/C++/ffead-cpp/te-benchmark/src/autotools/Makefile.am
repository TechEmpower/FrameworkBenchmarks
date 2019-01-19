AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

AM_CPPFLAGS=-I"../../../../include" -I"../../include"

packageIdentifier=${PACKAGE_NAME}-${PACKAGE_VERSION}-bin
distdir=${PACKAGE_NAME}-${PACKAGE_VERSION}-src
fprefix=../../../../${packageIdentifier}
prefix=${abs_builddir}

lib_LTLIBRARIES = libte_benchmark.la
libte_benchmark_la_SOURCES = ../TeBkFortune.cpp \
				../TeBkFortuneTemplate.cpp \
				../TeBkMessage.cpp \
				../TeBkRestController.cpp \
				../TeBkWorld.cpp

libte_benchmark_la_LDFLAGS = -no-undefined 
libte_benchmark_la_LIBADD = -L"${fprefix}/lib" -lffead_common -lffead_framework

#dist_noinst_SCRIPTS = autogen.sh