FROM lganzzzo/ubuntu-cmake-mimalloc

#---------------------------------------------------------------
# install oatpp

WORKDIR /test

RUN git clone https://github.com/oatpp/oatpp

WORKDIR /test/oatpp

RUN git checkout 4cd37af26ffa55231f11649106a1bb33a3244cd1

WORKDIR /test/oatpp/build

RUN cmake -DOATPP_DISABLE_ENV_OBJECT_COUNTERS=ON -DCMAKE_BUILD_TYPE=Release -DOATPP_BUILD_TESTS=OFF ..
RUN make install

#---------------------------------------------------------------
# build test app

ADD src-async /test/src-async

WORKDIR /test/src-async/build

RUN cmake -DCMAKE_BUILD_TYPE=Release ..
RUN make

EXPOSE 8000

CMD ./oatpp-async-test
