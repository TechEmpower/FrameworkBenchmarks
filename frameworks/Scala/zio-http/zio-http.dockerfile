FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_2.13.16

WORKDIR /zhttp
COPY src src
COPY project project
COPY build.sbt build.sbt
RUN sbt assembly

EXPOSE 8080
CMD java -Xms2G -Xmx2G -server -jar /zhttp/target/scala-2.13/zio-http-assembly-1.0.0.jar