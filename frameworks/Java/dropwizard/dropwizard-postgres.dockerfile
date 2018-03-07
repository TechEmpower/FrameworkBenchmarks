FROM tfb/maven:latest
ADD ./ /dropwizard
WORKDIR /dropwizard
RUN mvn clean package -P postgres
CMD java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world-postgres.yml
