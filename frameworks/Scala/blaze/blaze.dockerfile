FROM tfb/sbt:latest

COPY ./ ./

RUN sbt 'oneJar' -batch

CMD java -jar target/scala-2.11/blaze*one-jar.jar
