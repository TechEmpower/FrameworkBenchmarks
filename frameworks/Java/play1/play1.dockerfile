FROM tfb/java:latest
ADD ./ /play1
WORKDIR /play1
RUN wget -nv https://downloads.typesafe.com/play/1.5.0/play-1.5.0.zip
RUN unzip -q play-1.5.0.zip
RUN apt-get install -y python
CMD play-1.5.0/play run --%prod
