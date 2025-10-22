FROM buildpack-deps:noble

RUN apt-get update -yqq && apt-get install -yqq software-properties-common unzip cmake

RUN apt-get install -yqq g++-14
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-14 50

ENV POCO_VERSION 1.13.3
ENV POCO_HOME /poco

WORKDIR ${POCO_HOME}
RUN wget https://pocoproject.org/releases/poco-${POCO_VERSION}/poco-${POCO_VERSION}-all.zip
RUN unzip poco-${POCO_VERSION}-all.zip
RUN mv ./poco-${POCO_VERSION}-all/* ./

RUN ./configure --no-tests --no-samples
RUN make --quiet PageCompiler-libexec XML-libexec JSON-libexec

ENV LD_LIBRARY_PATH ${POCO_HOME}/lib/Linux/x86_64

COPY benchmark.cpp benchmark.cpp

RUN g++-14 \
    -O3 \
    -DNDEBUG \
    -std=c++17 \
    -o \
    poco \
    benchmark.cpp \
    -I${POCO_HOME}/Foundation/include \
    -I${POCO_HOME}/Util/include \
    -I${POCO_HOME}/Net/include \
    -L${POCO_HOME}/lib/Linux/x86_64 \
    -lPocoNet \
    -lPocoUtil \
    -lPocoFoundation \
    -lPocoXML \
    -lPocoJSON

EXPOSE 8080

CMD ./poco 8080 $(nproc)
