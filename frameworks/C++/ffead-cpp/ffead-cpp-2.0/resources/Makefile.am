AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

AM_CPPFLAGS=-I"../../../../include" -I"../../include"

prefix=${abs_builddir}

lib_LTLIBRARIES = libAPP_NAME.la
libAPP_NAME_la_SOURCES = 

libAPP_NAME_la_LDFLAGS = -no-undefined 
libAPP_NAME_la_LIBADD = -L"../../../../src/autotools/.libs" -lffead_common -lffead_framework FAPP_LIBS

#dist_noinst_SCRIPTS = autogen.sh