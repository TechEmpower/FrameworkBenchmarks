FROM techempower/sbt:0.1

COPY ./ ./

RUN sbt assembly

CMD java -jar target/scala-2.11/scruffy-benchmark-assembly-11.0.jar -Dhostname=tfb-database
