AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

AM_CPPFLAGS=-I"../../include" -I/home/vagrant/FrameworkBenchmarks/frameworks/C++/ffead-cpp/ffead-cpp-2.0/web/markers/include/ -I/home/vagrant/FrameworkBenchmarks/frameworks/C++/ffead-cpp/ffead-cpp-2.0/web/default/include/ -I/home/vagrant/FrameworkBenchmarks/frameworks/C++/ffead-cpp/ffead-cpp-2.0/web/te-benchmark/include/ -I/home/vagrant/FrameworkBenchmarks/frameworks/C++/ffead-cpp/ffead-cpp-2.0/web/oauthApp/include/ -I/home/vagrant/FrameworkBenchmarks/frameworks/C++/ffead-cpp/ffead-cpp-2.0/web/flexApp/include/ 

prefix=${abs_builddir}

lib_LTLIBRARIES = libinter.la
libinter_la_SOURCES = ../ReflectorInterface.cpp ../SerializeInterface.cpp ../AjaxInterface.cpp ../ApplicationInterface.cpp ../WsInterface.cpp 
libinter_la_LIBADD = -L"../../lib" -lffead_common -lffead_framework -lmarkers -ldefault -lte_benchmark -loauthApp -lflexApp 
libinter_la_LDFLAGS = -avoid-version -no-undefined -export-dynamic

#if BUILT_WITH_CONFGURE
lib_LTLIBRARIES += libdinter.la
libdinter_la_SOURCES = ../DCPInterface.cpp ../TemplateInterface.cpp 
libdinter_la_LIBADD = -L"../../lib" -lffead_common -lffead_framework -lmarkers -ldefault -lte_benchmark -loauthApp -lflexApp 
libdinter_la_LDFLAGS = -avoid-version -no-undefined -export-dynamic
#endif

dist_noinst_SCRIPTS = autogen.sh
