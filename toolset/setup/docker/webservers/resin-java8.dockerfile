FROM tfb/java8:latest

RUN mkdir /resin
WORKDIR /resin
RUN curl -sL http://www.caucho.com/download/resin-4.0.55.tar.gz | tar xz

ENV RESIN_HOME=/resin/resin-4.0.55

# Remove the default app so that frameworks using Resin don't have to.
RUN rm -rf ${RESIN_HOME}/webapps/*
