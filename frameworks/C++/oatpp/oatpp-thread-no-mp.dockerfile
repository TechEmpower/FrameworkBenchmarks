FROM lganzzzo/ubuntu-cmake

#---------------------------------------------------------------
# install oatpp

WORKDIR /test

RUN git clone https://github.com/oatpp/oatpp

WORKDIR /test/oatpp

RUN git checkout f24a2247098d02975869e8d90a059770ddca8df5

WORKDIR /test/oatpp/build

RUN cmake -DOATPP_DISABLE_ENV_OBJECT_COUNTERS=ON -DOATPP_DISABLE_POOL_ALLOCATIONS=ON -DCMAKE_BUILD_TYPE=Release -DOATPP_BUILD_TESTS=OFF ..
RUN make install

#---------------------------------------------------------------
# build test app

ADD src-thread /test/src-thread

WORKDIR /test/src-thread/build

RUN cmake -DCMAKE_BUILD_TYPE=Release ..
RUN make

EXPOSE 8000 8000

CMD ./oatpp-thread-test
