FROM tfb/maven:latest
ADD ./ /grizzly-jersey
WORKDIR /grizzly-jersey
RUN mvn clean package
CMD java -jar target/grizzly-jersey-example.jar
