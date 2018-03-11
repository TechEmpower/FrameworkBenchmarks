FROM tfb/maven:latest
ADD ./ /vertx-web
WORKDIR /vertx-web
RUN mvn clean package
