FROM techempower/base:0.2

RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test
RUN apt update -y
RUN apt install -y g++-4.8
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.8 50
