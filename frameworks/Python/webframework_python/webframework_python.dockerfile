FROM ubuntu:24.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive
ENV PIP_BREAK_SYSTEM_PACKAGES=1

RUN apt update
RUN apt install -y gcc g++ cmake git unzip zip wget ninja-build uuid-dev python3-dev python3-pip python3-venv
RUN python3 -m pip install build

WORKDIR /opt

RUN git clone https://github.com/LazyPanda07/WebFramework -b v3.3.1 --recursive

WORKDIR /opt/WebFramework

RUN mkdir build

WORKDIR /opt/WebFramework/build

RUN cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/opt/WebFrameworkLibrary -DCMAKE_CXX_FLAGS="-march=native" -DCMAKE_C_FLAGS="-march=native" \
	-DBUILD_CSHARP_API=OFF -DBUILD_PYTHON_API=ON -DBUILD_CC_API=OFF -DWITH_DOTNET_EXECUTORS=OFF -DWITH_PYTHON_EXECUTORS=ON -G "Ninja" ..
RUN cmake --build . -j
RUN cmake --install .

FROM ubuntu:24.04 AS deploy

ENV DEBIAN_FRONTEND=noninteractive
ENV PIP_BREAK_SYSTEM_PACKAGES=1
ENV LD_LIBRARY_PATH=/opt/WebFrameworkLibrary

RUN apt update
RUN apt install -y python3-dev python3-pip

COPY --from=builder /opt/WebFrameworkLibrary/ /opt/WebFrameworkLibrary
COPY benchmark/ /opt/benchmark

RUN python3 -m pip install /opt/WebFrameworkLibrary/api/python_api/dist/*.whl

WORKDIR /opt/benchmark

EXPOSE 8080
ENTRYPOINT ["python3", "main.py"]
