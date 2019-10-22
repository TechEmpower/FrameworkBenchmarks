FROM buildpack-deps:bionic

ENV DEBIAN_FRONTEND noninteractive
ENV TFVER=1.26.0

RUN apt-get update -yqq && apt-get install -yqq software-properties-common unzip cmake
RUN apt-get install -yqq g++ gcc libjemalloc-dev qt5-qmake qt5-default qtbase5-dev \
    qtbase5-dev-tools libqt5sql5 libqt5sql5-mysql libqt5sql5-psql libqt5qml5 libqt5xml5 \
    qtdeclarative5-dev libqt5quick5 libqt5quickparticles5 libqt5gui5 libqt5printsupport5 \
    libqt5widgets5 libqt5opengl5-dev libqt5quicktest5

RUN wget -q https://github.com/treefrogframework/treefrog-framework/archive/v${TFVER}.tar.gz
RUN tar xf v${TFVER}.tar.gz
RUN cd treefrog-framework-${TFVER} && \
    ./configure && \
    cd src && \
    make -j4 && \
    make install && \
    cd ../tools && \
    make -j4 && \
    make install

WORKDIR /workspace
COPY ./ ./

RUN sed -i 's|DriverType=.*|DriverType=QPSQL|g' config/database.ini
RUN sed -i 's|MultiProcessingModule=.*|MultiProcessingModule=thread|g' config/application.ini

# 1. Generate Makefile
RUN qmake -r CONFIG+=release

# 2. Compile applicaton
RUN make

# 3. Start TreeFrog
CMD treefrog /workspace
