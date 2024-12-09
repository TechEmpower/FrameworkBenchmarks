FROM buildpack-deps:focal

RUN apt-get update -yqq > /dev/null && apt-get install -yqq software-properties-common unzip cmake > /dev/null
RUN apt-get install -yqq build-essential libgoogle-perftools-dev git-core libssl-dev zlib1g-dev > /dev/null

EXPOSE 5006
RUN mkdir build_ltio

ADD CMakeLists.txt build_ltio/
ADD http_benchmark.cc build_ltio/

WORKDIR /build_ltio
RUN pwd; git clone -b benchmark https://github.com/echoface/ltio.git --recurse

RUN ls; cmake .; make --quiet

CMD ./benchmark_server $(nproc)
