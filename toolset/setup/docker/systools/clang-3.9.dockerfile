FROM techempower/base:0.2

RUN add-apt-repository -s "deb http://apt.llvm.org/`lsb_release -cs`/ llvm-toolchain-`lsb_release -cs`-3.9 main"
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
RUN apt -yq update
RUN apt install -qqy clang-3.9 lldb-3.9
