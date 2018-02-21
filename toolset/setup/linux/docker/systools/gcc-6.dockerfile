FROM tfb:latest

RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y && \
    apt-get -yq update && \
    apt-get install -qqy gcc-6 g++-6
