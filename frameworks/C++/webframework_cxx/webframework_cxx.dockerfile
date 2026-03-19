FROM ubuntu:24.04 AS builder

ENV WEB_FRAMEWORK_SDK=/opt/WebFrameworkLibrary

RUN apt update
RUN apt install -y gcc g++ cmake git unzip zip wget ninja-build uuid-dev

WORKDIR /opt

RUN git clone https://github.com/LazyPanda07/WebFramework -b v3.3.1 --recursive
COPY benchmark/ ./benchmark

WORKDIR /opt/WebFramework

RUN mkdir build

WORKDIR /opt/WebFramework/build

RUN cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/opt/WebFrameworkLibrary -DCMAKE_CXX_FLAGS="-march=native" -DCMAKE_C_FLAGS="-march=native" \
	-DBUILD_CSHARP_API=OFF -DBUILD_PYTHON_API=OFF -DBUILD_CC_API=OFF -DWITH_DOTNET_EXECUTORS=OFF -DWITH_PYTHON_EXECUTORS=OFF -G "Ninja" ..
RUN cmake --build . -j
RUN cmake --install .

WORKDIR /opt/benchmark

RUN mkdir build

WORKDIR /opt/benchmark/build

RUN cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/opt/app -DCMAKE_CXX_FLAGS="-march=native" -DCMAKE_C_FLAGS="-march=native" -G "Ninja" ..
RUN cmake --build . -j
RUN cmake --install .

FROM ubuntu:24.04 AS deploy

COPY --from=builder /opt/app ./

EXPOSE 8080
CMD LD_LIBRARY_PATH=$(pwd) ./benchmark
