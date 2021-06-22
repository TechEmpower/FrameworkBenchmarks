FROM buildpack-deps:focal

RUN apt-get update -yqq && apt-get install -yqq software-properties-common unzip cmake
RUN apt-get install -yqq build-essential libgoogle-perftools-dev git-core

RUN git clone https://github.com/echoface/ltio.git --recurse

WORKDIR /ltio
RUN git branch -a; git pull; git checkout benchmark
RUN mkdir build_ltio
RUN cd build_ltio; cmake ..; make

EXPOSE 5006
#CMD cd build_ltio; ./bin/simple_ltserver >/dev/null 2>&1 $(nproc)
CMD cd build_ltio; ./bin/simple_ltserver $(nproc)
