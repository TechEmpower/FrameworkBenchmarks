FROM tfb/maven:latest
ADD ./ /vertx
WORKDIR /vertx
RUN mvn clean package
