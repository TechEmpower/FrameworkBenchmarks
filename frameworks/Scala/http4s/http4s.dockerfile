FROM techempower/sbt:0.1

COPY ./ ./

RUN sbt 'oneJar' -batch

CMD java -jar target/scala-2.12/http4s*one-jar.jar "TFB-database"
