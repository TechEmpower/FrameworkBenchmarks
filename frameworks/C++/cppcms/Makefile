LIBS=-lcppcms -lbooster -lcppdb

CXXFLAGS += -O3 -s -DNDEBUG -std=c++11 -I$(CPPCMS_HOME)/include -I$(CPPDB_HOME)/include -L$(CPPCMS_HOME)/lib -L$(CPPDB_HOME)/lib

CSRC =  src/fortunes_view.cpp\
	src/main.cpp\
	src/test_db_base.cpp\
	src/test_db.cpp\
	src/test_fortunes.cpp\
	src/test_simple.cpp

all: clean main

main: $(CSRC) src/fortunes_view.cpp
	$(CXX) $(CXXFLAGS) -Wall $(CSRC) -o mycppcms $(LIBS)

src/fortunes_view.cpp: src/fortunes.tmpl src/data.h
	$(CPPCMS_HOME)/bin/cppcms_tmpl_cc src/fortunes.tmpl -o src/fortunes_view.cpp

clean:
	rm -fr mycppcms *.exe src/fortunes_view.cpp cppcms_rundir

