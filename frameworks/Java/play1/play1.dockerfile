FROM openjdk:10-jdk
WORKDIR /play1
COPY app app
COPY conf conf
COPY public public
COPY test test
RUN wget -nv https://downloads.typesafe.com/play/1.5.2/play-1.5.2.zip
RUN unzip -q play-1.5.2.zip
RUN apt-get install -yqq python

EXPOSE 8080

CMD ["play-1.5.2/play", "run", "--%prod"]
