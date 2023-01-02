FROM buildpack-deps:jammy

ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NOWARNINGS yes
ENV TFVER=2.6.0

RUN apt-get update -yqq && apt-get upgrade -yq && \
    apt-get install -yqq --no-install-recommends software-properties-common unzip wget libjemalloc-dev \
    qmake6 qt6-base-dev qt6-base-dev-tools qt6-tools-dev-tools qt6-declarative-dev libqt6sql6-mysql \
    libqt6sql6-psql libqt6sql6-odbc libqt6sql6-sqlite libqt6core6 libqt6qml6 libqt6xml6 libpq5 libodbc1 \
    libmongoc-dev libbson-dev gcc g++ clang make cmake pkg-config
RUN rm -f /usr/bin/qmake; ln -sf /usr/bin/qmake6 /usr/bin/qmake

WORKDIR /usr/src
RUN wget -q https://github.com/treefrogframework/treefrog-framework/archive/v${TFVER}.tar.gz
RUN tar xf v${TFVER}.tar.gz
RUN cd treefrog-framework-${TFVER} && \
    ./configure --enable-shared-mongoc --spec=linux-clang && \
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
RUN sed -i 's|MultiProcessingModule=.*|MultiProcessingModule=epoll|g' config/application.ini

EXPOSE 8080

# 3. Start TreeFrog
CMD treefrog /workspace
