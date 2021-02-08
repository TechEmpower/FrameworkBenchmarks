FROM buildpack-deps:focal

ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NOWARNINGS yes
ENV TFVER=1.31.1

RUN apt-get update -yqq && apt-get upgrade -yq && apt-get install -yqq --no-install-recommends \
    software-properties-common unzip wget make cmake gcc clang libjemalloc-dev qt5-qmake qt5-default qtbase5-dev \
    qtbase5-dev-tools libqt5sql5 libqt5sql5-mysql libqt5sql5-psql libqt5qml5 libqt5xml5 \
    qtdeclarative5-dev libqt5quick5 libqt5quickparticles5 libqt5gui5 libqt5printsupport5 \
    libqt5widgets5 libqt5opengl5-dev libqt5quicktest5 libqt5sql5-sqlite libsqlite3-dev libmongoc-dev libbson-dev \
    redis-server

WORKDIR /usr/src
RUN wget -q https://github.com/treefrogframework/treefrog-framework/archive/v${TFVER}.tar.gz
RUN tar xf v${TFVER}.tar.gz
RUN cd treefrog-framework-${TFVER} && \
    ./configure --spec=linux-clang && \
    cd src && \
    make -j4 && \
    make install && \
    cd ../tools && \
    make -j4 && \
    make install

WORKDIR /workspace
COPY ./ ./

# 1. Generate Makefile
RUN qmake -r CONFIG+=release -spec linux-clang

# 2. Compile applicaton
RUN make
RUN sed -i 's|DriverType=.*|DriverType=QPSQL|g' config/database.ini
RUN sed -i 's|MultiProcessingModule=.*|MultiProcessingModule=thread|g' config/application.ini

EXPOSE 8080

# 3. Start TreeFrog
CMD service redis-server start && \
    treefrog /workspace
