FROM tfb/base:latest

RUN wget https://github.com/commercialhaskell/stack/releases/download/v1.6.5/stack-1.6.5-linux-x86_64.tar.gz

RUN tar xzf stack-1.6.5-linux-x86_64.tar.gz

RUN mv stack-1.6.5-linux-x86_64.tar.gz stack

RUN apt install -yqq make automake gcc libgmp3-dev
