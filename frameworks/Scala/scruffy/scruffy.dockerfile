FROM tfb/sbt:latest

COPY ./ ./

RUN sbt assembly

CMD java -jar target/scala-2.11/scruffy-benchmark-assembly-11.0.jar -Dhostname=TFB-database
