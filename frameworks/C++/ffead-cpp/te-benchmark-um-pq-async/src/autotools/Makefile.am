AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

AM_CPPFLAGS=-I"../../../../include" -I"../../include"

packageIdentifier=${PACKAGE_NAME}-${PACKAGE_VERSION}-bin
distdir=${PACKAGE_NAME}-${PACKAGE_VERSION}-src
fprefix=../../../../${packageIdentifier}
prefix=${abs_builddir}

lib_LTLIBRARIES = libte-benchmark-um-pq-async.la
libte_benchmark_um_pq_async_la_SOURCES = ../TeBkUmLpqAsync.cpp

libte_benchmark_um_pq_async_la_LDFLAGS = -no-undefined 
libte_benchmark_um_pq_async_la_LIBADD = -L"${fprefix}/lib" -lffead-modules -lffead-framework

#dist_noinst_SCRIPTS = autogen.sh