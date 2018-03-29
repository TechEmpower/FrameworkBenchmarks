FROM tfb/sbt:latest

ADD ./ /spray
WORKDIR /spray

RUN sbt assembly -batch

CMD java -jar target/scala-2.12/spray-benchmark-assembly-1.0.jar
