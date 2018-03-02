FROM tfb/maven:latest
ADD ./ /ninja-standalone
WORKDIR /ninja-standalone
RUN mvn clean compile assembly:single
CMD java -Dninja.port=8080 -jar target/ninja-standalone-0.0.1-SNAPSHOT-jar-with-dependencies.jar
