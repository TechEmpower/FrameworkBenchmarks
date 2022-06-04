FROM adoptopenjdk/openjdk13

RUN apt-get update -yqq
RUN apt-get install -yqq gnupg git

# Install sbt
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update -yqq
RUN apt-get install -yqq sbt

WORKDIR /scalene
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch

EXPOSE 8080

CMD ["java", "-server", "-Xmx2G", "-Xms2G", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "target/scala-2.13/scalene-benchmark-assembly-0.1.0-SNAPSHOT.jar"]
