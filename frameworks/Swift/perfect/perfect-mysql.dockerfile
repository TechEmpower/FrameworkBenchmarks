FROM swift:4.1

ADD . /perfect
WORKDIR /perfect
RUN apt-get update -yqq && apt-get install -yqq libpq-dev && apt-get install -y xsltproc docbook-xsl uuid-dev clang pkg-config libicu-dev libpython2.7 libxml2-dev wget git libssl-dev uuid-dev libsqlite3-dev libpq-dev libmysqlclient-dev libbson-dev libmongoc-dev libcurl4-openssl-dev && apt-get -y install libmysqlclient-dev libmongoc-1.0-0 libbson-1.0
RUN swift build

EXPOSE 8080

CMD .build/debug/Perfect-MySQL
