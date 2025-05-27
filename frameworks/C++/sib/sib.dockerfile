FROM ubuntu:24.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    git \
    cmake \
    gcc \
    g++ \
    ninja-build

# Set compiler environment
ENV CC=gcc
ENV CXX=g++
ENV AR=gcc-ar
ENV RANLIB=gcc-ranlib

# Clone SIB repository with submodules
WORKDIR /sib
RUN git clone --depth 1 https://github.com/PooyaEimandar/sib.git -b sib-seastar .

RUN cd ./dep && ./seastar.sh release && cd ..

RUN cmake -B build -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_FLAGS=-flto \
 && cmake --build build 

# Run the sib server 
EXPOSE 8080
CMD ["./build/sib"]