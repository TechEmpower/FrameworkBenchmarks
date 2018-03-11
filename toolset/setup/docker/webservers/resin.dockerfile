FROM tfb/java:latest

RUN mkdir /resin
WORKDIR /resin
RUN curl -s http://www.caucho.com/download/resin-4.0.55.tar.gz | tar xz
WORKDIR /resin/resin-4.0.55
RUN ./configure
RUN make
RUN make install

# Remove the default app so that frameworks using Resin don't have to.
RUN rm -rf /var/resin/webapps/*

ENV RESIN_HOME=/resin/resin-4.0.55
