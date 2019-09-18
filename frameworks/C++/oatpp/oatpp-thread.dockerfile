FROM lganzzzo/ubuntu-cmake

#---------------------------------------------------------------
# install oatpp

WORKDIR /test

RUN git clone https://github.com/oatpp/oatpp

WORKDIR /test/oatpp

RUN git checkout 73d9523a5ac627609f8d409ce46305404cfbae0e

WORKDIR /test/oatpp/build

RUN cmake -DOATPP_DISABLE_ENV_OBJECT_COUNTERS=ON -DCMAKE_BUILD_TYPE=Release -DOATPP_BUILD_TESTS=OFF ..
RUN make install

#---------------------------------------------------------------
# build test app

ADD src-thread /test/src-thread

WORKDIR /test/src-thread/build

RUN cmake -DCMAKE_BUILD_TYPE=Release ..
RUN make

EXPOSE 8000 8000

CMD ./oatpp-thread-test
