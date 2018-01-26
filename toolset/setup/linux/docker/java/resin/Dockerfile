FROM tfb/java:latest

RUN apt-get install -qqy -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    curl build-essential automake

ENV RESIN_HOME=/resin-4.0.55

RUN curl -sLO http://www.caucho.com/download/resin-4.0.55.tar.gz
RUN tar xf resin-4.0.55.tar.gz
RUN cd resin-4.0.55; ./configure; make; make install