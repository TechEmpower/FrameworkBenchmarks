FROM maven:3.6.1-jdk-11-slim as maven

WORKDIR /gemini

COPY servlet/src src
COPY servlet/pom.xml pom.xml

RUN mvn -q compile
RUN mv src/main/webapp/WEB-INF/configuration/gemini-postgres.conf src/main/webapp/WEB-INF/configuration/Base.conf
RUN mvn -q war:war

FROM openjdk:11.0.7-slim
RUN apt update -qqy && apt install -yqq curl > /dev/null

WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.63.tar.gz | tar xz --strip-components=1
# Taken from buildpack-deps:stretch - Resin compilation requires JAVA_HOME
# also added several missing dependencies
RUN DEBIAN_FRONTEND=noninteractive apt-get update -yqq &> /dev/null; \
  DEBIAN_FRONTEND=noninteractive apt-get install -yqq --no-install-recommends \
  autoconf \
  automake \
  build-essential \
  bzip2 \
  dpkg-dev \
  file \
  g++ \
  gcc \
  gcc-multilib \
  imagemagick \
  libbz2-dev \
  libc6-dev \
  libcurl4-openssl-dev \
  libdb-dev \
  libevent-dev \
  libffi-dev \
  libgdbm-dev \
  libgeoip-dev \
  libglib2.0-dev \
  libgmp-dev \
  libjpeg-dev \
  libkrb5-dev \
  liblzma-dev \
  libmagickcore-dev \
  libmagickwand-dev \
  libncurses5-dev \
  libncursesw5-dev \
  libpng-dev \
  libpq-dev \
  libreadline-dev \
  libsqlite3-dev \
  libssl-dev \
  libtool \
  libwebp-dev \
  libxml2-dev \
  libxslt-dev \
  libyaml-dev \
  linux-libc-dev \
  linux-headers-amd64 \
  make \
  patch \
  unzip \
  xz-utils \
  zlib1g-dev > /dev/null

RUN ./configure --prefix=`pwd` --enable-64bit -q &> /dev/null
RUN make -s &> /dev/null
RUN make install -s &> /dev/null
RUN rm -rf webapps/*
RUN mkdir logs
COPY --from=maven /gemini/target/HelloWorld-0.0.1.war webapps/ROOT.war

EXPOSE 8080

CMD ["java", "-jar", "lib/resin.jar", "console"]
