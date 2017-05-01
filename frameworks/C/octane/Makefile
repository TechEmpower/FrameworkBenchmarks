projectpath = ${CURDIR}
octane_path = ${projectpath}/lib/octane

ifeq ($(OS),Windows_NT)
		OPERATING_SYSTEM = WINDOWS
    CCFLAGS += -D WIN32
    ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
        CCFLAGS += -D AMD64
    else
        ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
            CCFLAGS += -D AMD64
        endif
        ifeq ($(PROCESSOR_ARCHITECTURE),x86)
            CCFLAGS += -D IA32
        endif
    endif
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
				OPERATING_SYSTEM = LINUX
				PLATFORM_TARGET = linux
        CCFLAGS += -D LINUX
    endif
    ifeq ($(UNAME_S),Darwin)
				OPERATING_SYSTEM = OSX
				PLATFORM_TARGET = osx
        CCFLAGS += -D OSX
    endif
    UNAME_P := $(shell uname -p)
    ifeq ($(UNAME_P),x86_64)
        CCFLAGS += -D AMD64
    endif
    ifneq ($(filter %86,$(UNAME_P)),)
        CCFLAGS += -D IA32
    endif
    ifneq ($(filter arm%,$(UNAME_P)),)
        CCFLAGS += -D ARM
    endif
endif

all: deps $(PLATFORM_TARGET)

target: all

osx: deps
	if [ ! -d "build" ]; then mkdir -p build; fi
	if [ ! -d "build/Makefile" ]; then cd build;cmake -DCMAKE_OSX_ARCHITECTURES=x86_64 ..; fi
	cmake --build ./build --target all --config Debug -- -j 10

xcode: deps
	if [ ! -d "build" ]; then mkdir -p build; fi
	if [ ! -d "build/techempower_benchmarks.xcodeproj" ]; then cd build;cmake -DCMAKE_OSX_ARCHITECTURES=x86_64 -G Xcode ..; fi
	cd build;xcodebuild -project techempower_benchmarks.xcodeproj/

$(octane_path)/build/liboctane.a:
	if [ ! -d "$(octane_path)" ]; then git clone https://github.com/simongui/octane.git $(octane_path); fi
	cd $(octane_path);make

deps: $(octane_path)/build/liboctane.a
