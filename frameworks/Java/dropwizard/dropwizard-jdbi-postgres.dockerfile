FROM techempower/maven:0.1

ADD ./ /dropwizard
WORKDIR /dropwizard
RUN mvn clean package -P postgres,jdbi
CMD java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world-jdbi-postgres.yml
