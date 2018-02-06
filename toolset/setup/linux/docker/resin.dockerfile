FROM java:latest

ENV RESIN_HOME=/resin-4.0.55

RUN curl -sLO http://www.caucho.com/download/resin-4.0.55.tar.gz
RUN tar xf resin-4.0.55.tar.gz
RUN cd resin-4.0.55; ./configure; make; make install