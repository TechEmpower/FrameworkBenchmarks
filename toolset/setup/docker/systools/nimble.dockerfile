FROM tfb/nim:latest
  
ENV NIMBLE_VERSION="0.6.2"

RUN cd $NIM_HOME && \
    wget https://github.com/nim-lang/nimble/archive/v$NIMBLE_VERSION.tar.gz && \
    tar xf v$NIMBLE_VERSION.tar.gz && \
    mv nimble-$NIMBLE_VERSION nimble && \
    cd nimble && \
    ../bin/nim c src/nimble && \
    mv src/nimble ../bin/
