FROM tfb/nginx:latest

ENV NIM_VERSION="0.11.2"
ENV NIM_CSOURCES="6bf2282"

RUN wget https://github.com/nim-lang/Nim/archive/v$NIM_VERSION.tar.gz
RUN tar xvf v$NIM_VERSION.tar.gz
RUN mv Nim-$NIM_VERSION nim

RUN cd nim && \
    git clone git://github.com/nim-lang/csources.git && \
    cd csources && \
    git checkout $NIM_CSOURCES && \
    sh build.sh && \
    cd .. && \

    bin/nim c koch && \
    ./koch boot -d:release

ENV NIM_HOME=/nim
ENV PATH=${NIM_HOME}/bin:${PATH}
