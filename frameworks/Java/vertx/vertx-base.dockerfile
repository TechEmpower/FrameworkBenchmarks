FROM techempower/maven:0.1

ADD ./ /vertx
WORKDIR /vertx
RUN mvn clean package
