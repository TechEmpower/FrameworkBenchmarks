FROM swift:4.1

ADD . /perfect
WORKDIR /perfect
RUN apt update -yqq && apt install -yqq libpq-dev && apt install -y xsltproc docbook-xsl uuid-dev
# RUN apt-get -y install cmake libssl-dev libsasl2-dev
RUN apt-get -y install clang pkg-config libicu-dev libpython2.7 libxml2-dev wget git libssl-dev uuid-dev libsqlite3-dev libpq-dev libmysqlclient-dev libbson-dev libmongoc-dev libcurl4-openssl-dev
RUN apt-get -y install libmysqlclient-dev && apt-get -y install libmongoc-1.0-0 && apt-get -y install libbson-1.0
RUN swift build
CMD .build/debug/Perfect-MongoDB