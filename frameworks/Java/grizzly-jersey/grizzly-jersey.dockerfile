FROM techempower/maven:0.1

ADD ./ /grizzly-jersey
WORKDIR /grizzly-jersey
RUN mvn clean package
CMD java -jar target/grizzly-jersey-example.jar
