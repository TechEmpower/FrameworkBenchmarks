FROM buildpack-deps:xenial

ENV IROOT=/installs
ENV FFEAD_CPP_PATH=${IROOT}/ffead-cpp-2.0
ENV PATH=${FFEAD_CPP_PATH}:${PATH}

RUN mkdir /installs

RUN apt update -yqq && apt install -yqq unzip uuid-dev odbc-postgresql unixodbc unixodbc-dev

WORKDIR $IROOT

# libmyodbc has been removed from apt

RUN wget -q http://www.mirrorservice.org/sites/ftp.mysql.com/Downloads/Connector-ODBC/5.3/mysql-connector-odbc-5.3.10-linux-ubuntu16.04-x86-64bit.tar.gz
RUN tar xf mysql-connector-odbc-5.3.10-linux-ubuntu16.04-x86-64bit.tar.gz
RUN mkdir -p /usr/lib/x86_64-linux-gnu/odbc
RUN mv mysql-connector-odbc-5.3.10-linux-ubuntu16.04-x86-64bit/lib/libmyodbc5* /usr/lib/x86_64-linux-gnu/odbc/
RUN mysql-connector-odbc-5.3.10-linux-ubuntu16.04-x86-64bit/bin/myodbc-installer -d -a -n "MySQL" -t "DRIVER=/usr/lib/x86_64-linux-gnu/odbc/libmyodbc5w.so;"

# mongocdriver also used in all tests

RUN wget -q https://github.com/mongodb/mongo-c-driver/releases/download/1.4.0/mongo-c-driver-1.4.0.tar.gz
RUN tar xf mongo-c-driver-1.4.0.tar.gz
RUN cd mongo-c-driver-1.4.0/ && \
    ./configure --prefix=${IROOT} --libdir=${IROOT} --disable-automatic-init-and-cleanup && \
    make && make install

WORKDIR /

COPY te-benchmark/ te-benchmark/
COPY ffead-cpp-framework.sh ./
COPY server.sh ./

RUN chmod 755 *.sh

RUN ./ffead-cpp-framework.sh

COPY ffead-cpp-httpd.sh ./

RUN chmod 755 *.sh

RUN ./ffead-cpp-httpd.sh

ENV PATH=${IROOT}/httpd/bin:${PATH}
