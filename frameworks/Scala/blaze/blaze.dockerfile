FROM tfb/sbt:latest

ADD ./ /blaze
WORKDIR /blaze

RUN sbt assembly -batch

CMD java -jar target/scala-2.12/blaze-assembly-1.0.jar
