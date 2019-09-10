FROM debian:9

RUN apt-get update

RUN apt-get install -yqq cmake
RUN apt-get install -yqq build-essential

RUN apt-get install -yqq git

#---------------------------------------------------------------
# install oatpp

WORKDIR /test

RUN git clone https://github.com/oatpp/oatpp

WORKDIR /test/oatpp

RUN git checkout 68bbb14ec4bb6b67cfb5917c7c1ed9201f82d341

WORKDIR /test/oatpp/build

RUN cmake -DCMAKE_BUILD_TYPE=Release -DOATPP_BUILD_TESTS=OFF ..
RUN make install

#---------------------------------------------------------------
# build test app

ADD src-async /test/src-async

WORKDIR /test/src-async/build

RUN cmake -DCMAKE_BUILD_TYPE=Release ..
RUN make

EXPOSE 8000 8000

CMD ./oatpp-async-test
