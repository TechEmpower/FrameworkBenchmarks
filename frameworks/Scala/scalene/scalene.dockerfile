FROM adoptopenjdk/openjdk13

ARG SBT_VERSION=1.3.7

RUN \
  apt-get update && \
  apt-get -y install git

# Install sbt
RUN \
  curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion

WORKDIR /scalene
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch

EXPOSE 8080

CMD ["java", "-server", "-Xmx2G", "-Xms2G", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "target/scala-2.13/scalene-benchmark-assembly-0.1.0-SNAPSHOT.jar"]
