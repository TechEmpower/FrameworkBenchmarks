FROM openjdk:11-jdk

RUN \
  echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
  echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
  curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
  apt-get update && \
  apt-get install sbt

WORKDIR /zhttp
COPY src src
COPY project project
COPY build.sbt build.sbt
RUN sbt assembly

EXPOSE 8080
CMD java -Xms2G -Xmx2G -server -jar /zhttp/target/scala-2.13/zio-http-assembly-1.0.0.jar