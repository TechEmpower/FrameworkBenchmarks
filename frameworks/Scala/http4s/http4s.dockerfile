FROM tfb/sbt:latest

ADD ./ /http4s
WORKDIR /http4s

RUN sbt assembly -batch

CMD java -jar target/scala-2.12/http4s-assembly-1.0.jar "TFB-database"
