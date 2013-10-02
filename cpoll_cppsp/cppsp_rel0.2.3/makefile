OPTFLAGS := -Ofast -march=native -flto
CXXFLAGS := $(OPTFLAGS) -Wall --std=c++0x -Wno-pmf-conversions -I./include
CXX := g++
all: cppsp_standalone cpoll.o cppsp.o socketd_bin socketd_cppsp socketd_proxy.so
cppsp_standalone: 
	$(CXX) cppsp_server/cppsp_standalone.C cpoll/all.C cppsp/all.C -o cppsp_standalone -lpthread -ldl -lrt $(CXXFLAGS)
socketd_bin:
	$(CXX) socketd/all.C cpoll/all.C -o socketd_bin -lpthread -ldl -lrt $(CXXFLAGS) 
socketd_cppsp:
	$(CXX) cppsp_server/socketd_cppsp.C cpoll/all.C cppsp/all.C -o socketd_cppsp -lpthread -ldl -lrt $(CXXFLAGS)
cpoll.o:
	$(CXX) cpoll/all.C -c -o cpoll.o $(CXXFLAGS) -fPIC
cppsp.o:
	$(CXX) cppsp/all.C -c -o cppsp.o $(CXXFLAGS) -fPIC
socketd_proxy.so:
	$(CXX) socketd_proxy.C cpoll/all.C -o socketd_proxy.so -ldl -lpthread --std=c++0x --shared -fPIC $(CXXFLAGS) 
clean:
	rm -f cpoll.o cppsp.o cppsp_standalone socketd_bin socketd_cppsp socketd_proxy.so
	rm -f www/*.cppsp.so www/*.cppsp.C www/*.cppsp.txt
	rm -f www/*.cppsm.so www/*.cppsm.C www/*.cppsm.txt
	rm -f www/*.html.so www/*.html.C www/*.html.txt

