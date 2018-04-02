FROM techempower/maven:0.1

ADD ./ /jooby
WORKDIR /jooby
RUN mvn clean package
CMD java \
    -server \
    -Xms512m \
    -Xmx2g \
    -jar target/jooby-1.0.jar
