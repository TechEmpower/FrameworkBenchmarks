FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /servlet
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM openjdk:11.0.3-stretch
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.61.tar.gz | tar xz --strip-components=1

# Taken from buildpack-deps:stretch - Resin compilation requires JAVA_HOME
# also added several missing dependencies
RUN set -ex; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
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
		zlib1g-dev

RUN ./configure --enable-64bit && make

RUN rm -rf webapps/*
COPY --from=maven /servlet/target/servlet.war webapps/ROOT.war
COPY resin.xml conf/resin.xml
RUN mkdir libexec64
RUN mv modules/c/src/resin_os/libresin_os.so libexec64/libresin_os.so

EXPOSE 8080

CMD ["java", "-jar", "lib/resin.jar", "console"]
