FROM ubuntu:22.04
RUN apt-get update -yqq && apt-get install -yqq software-properties-common wget unzip cmake git
RUN apt-get install -yqq gcc g++ openssl libssl-dev zlib1g-dev build-essential 

RUN  apt install brotli 
RUN  apt-get install mysql-client
RUN  apt-get install libmysqlclient-dev

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

WORKDIR /

RUN wget https://github.com/hggq/paozhu/releases/download/v1.5.6/benchmark.zip
RUN unzip benchmark.zip
RUN mv ./benchmark/* ./

RUN cmake . -B build -DCMAKE_BUILD_TYPE=Release 
RUN cmake --build build

EXPOSE 8888

CMD ./bin/paozhu 
