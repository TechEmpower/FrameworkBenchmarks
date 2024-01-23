FROM hseeberger/scala-sbt:11.0.12_1.5.5_2.13.6

WORKDIR /zhttp
COPY src src
COPY project project
COPY build.sbt build.sbt
RUN sbt assembly

EXPOSE 8080
CMD java -Xms2G -Xmx2G -server -jar /zhttp/target/scala-2.13/zio-http-assembly-1.0.0.jar