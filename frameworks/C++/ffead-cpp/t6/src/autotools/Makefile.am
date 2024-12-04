AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

AM_CPPFLAGS=-I"../../../../include" -I"../../include"

packageIdentifier=${PACKAGE_NAME}-${PACKAGE_VERSION}-bin
distdir=${PACKAGE_NAME}-${PACKAGE_VERSION}-src
fprefix=../../../../${packageIdentifier}
prefix=${abs_builddir}

lib_LTLIBRARIES = libt6.la
libt6_la_SOURCES = ../TeBkUmFpg.cpp

libt6_la_LDFLAGS = -no-undefined 
libt6_la_LIBADD = -L"${fprefix}/lib" -lffead-modules -lffead-framework

#dist_noinst_SCRIPTS = autogen.sh