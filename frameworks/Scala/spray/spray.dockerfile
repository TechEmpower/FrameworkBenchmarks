FROM tfb/sbt:latest

COPY ./ ./

RUN sbt assembly -batch

CMD java -jar target/scala-2.11/spray-benchmark-assembly-1.0.jar
