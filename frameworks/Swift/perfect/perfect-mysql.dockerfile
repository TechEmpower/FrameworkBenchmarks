FROM swift:4.1

ADD . /perfect
WORKDIR /perfect
RUN apt update -yqq && apt install -yqq libpq-dev && apt install -y xsltproc docbook-xsl uuid-dev
RUN apt-get -y install libmysqlclient-dev
RUN swift build
CMD .build/debug/Perfect-MySQL