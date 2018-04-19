FROM openjdk:10-jdk
WORKDIR /play1
COPY app app
COPY conf conf
COPY public public
COPY test test
RUN wget -nv https://downloads.typesafe.com/play/1.5.0/play-1.5.0.zip
RUN unzip -q play-1.5.0.zip
RUN apt install -yqq python
CMD ["play-1.5.0/play", "run", "--%prod"]
