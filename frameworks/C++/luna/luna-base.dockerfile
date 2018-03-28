FROM tfb/gcc-4.9:latest

RUN apt install -yqq python-dev python-pip cmake autoconf

# We're using conan 0.28.1 because, as of this writing, later versions of conan
# are incompatible with the luna framework, and luna is the only framework that
# uses conan.
RUN pip install conan==0.28.1

ADD ./ /luna
WORKDIR /luna

RUN CC=gcc-4.9 CXX=g++-4.9 conan install --build=missing -s compiler="gcc" -s compiler.version="4.9" .
RUN cmake . -DCMAKE_CXX_COMPILER=g++-4.9 -DCMAKE_CC_COMPILER=gcc-4.9
RUN cmake --build .
