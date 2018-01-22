FROM ubuntu:14.04

RUN apt-get update
RUN apt-get install -qqy software-properties-common
RUN add-apt-repository -y ppa:openjdk-r/ppa
RUN apt-get update
RUN apt-get install -qqy -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    openjdk-8-jdk ant curl build-essential automake

ENV JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
ENV RESIN_HOME=/resin-4.0.55
ENV PATH="/usr/lib/jvm/java-8-openjdk-amd64/bin:${PATH}"

RUN curl -sLO http://www.caucho.com/download/resin-4.0.55.tar.gz
RUN tar xf resin-4.0.55.tar.gz
RUN cd resin-4.0.55; ./configure; make; make install

ADD Docroot/ /gemini/Docroot
ADD Source/ /gemini/Source
ADD build.xml /gemini/
ADD ivy.xml /gemini/
ADD ivysettings.xml /gemini/

RUN cd /gemini; mkdir -p Docroot/WEB-INF/classes; ant resolve; ant compile

EXPOSE 8080

CMD resinctl -conf /gemini/Docroot/WEB-INF/resin.xml console