FROM ubuntu:22.04
RUN apt-get update -yqq && apt-get install -yqq apt-utils software-properties-common wget unzip cmake git
RUN apt-get install -yqq gcc g++ openssl libssl-dev zlib1g-dev build-essential locales

RUN apt-get -y install brotli libbrotli-dev 
RUN apt-get -y install libreadline-dev 
RUN apt-get -y install mysql-client
RUN apt-get -y install libmysqlclient-dev

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

COPY ./ ./
WORKDIR /

# RUN wget https://github.com/hggq/paozhu/releases/download/v1.5.8/benchmark.zip
RUN git clone https://github.com/hggq/paozhu
# RUN unzip benchmark.zip
RUN rm -Rf ./paozhu/controller
RUN rm -Rf ./paozhu/libs
RUN mkdir ./paozhu/libs
RUN mkdir ./paozhu/libs/types

RUN mkdir ./paozhu/controller
RUN mkdir ./paozhu/controller/include
RUN mkdir ./paozhu/controller/src

COPY ./paozhu_benchmark/controller/include/techempower.h ./paozhu/controller/include/
COPY ./paozhu_benchmark/controller/src/techempower.cpp ./paozhu/controller/src/

COPY ./paozhu_benchmark/libs/types/techempower_json.h ./paozhu/libs/types/
COPY ./paozhu_benchmark/libs/types/techempower_json_jsonreflect.cpp ./paozhu/libs/types/

COPY ./paozhu_benchmark/common/autocontrolmethod.hpp ./paozhu/common/
COPY ./paozhu_benchmark/common/reghttpmethod_pre.hpp ./paozhu/common/
COPY ./paozhu_benchmark/common/reghttpmethod.hpp ./paozhu/common/
COPY ./paozhu_benchmark/common/json_reflect_headers.h ./paozhu/common/

COPY ./paozhu_benchmark/conf/server.conf ./paozhu/conf/server.conf
COPY ./paozhu_benchmark/conf/orm.conf ./paozhu/conf/orm.conf
COPY ./paozhu_benchmark/CMakeLists.txt ./paozhu/CMakeLists.txt

WORKDIR /paozhu
RUN unzip asio.zip

RUN cmake . -B build -DCMAKE_BUILD_TYPE=Release 
RUN cmake --build build


EXPOSE 8888

CMD ./bin/paozhu 
