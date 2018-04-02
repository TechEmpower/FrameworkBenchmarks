FROM techempower/maven:0.1

ADD ./ /vertx-web
WORKDIR /vertx-web
RUN mvn clean package
