FROM buildpack-deps:xenial

RUN apt update -yqq && apt install -yqq software-properties-common
RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test
RUN apt update -yqq
RUN apt install -y g++-4.9 python-dev python-pip cmake autoconf
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 50

# We're using conan 0.28.1 because, as of this writing, later versions of conan
# are incompatible with the luna framework, and luna is the only framework that
# uses conan.
RUN pip install conan==0.28.1

ADD ./ /luna
WORKDIR /luna

RUN CC=gcc-4.9 CXX=g++-4.9 conan install --build=missing -s compiler="gcc" -s compiler.version="4.9" . > /dev/null
RUN cmake . -DCMAKE_CXX_COMPILER=g++-4.9 -DCMAKE_CC_COMPILER=gcc-4.9 > /dev/null
RUN cmake --build . > /dev/null

CMD /luna/bin/lunabench_thread 8080
