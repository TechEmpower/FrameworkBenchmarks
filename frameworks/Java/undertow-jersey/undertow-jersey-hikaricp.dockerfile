FROM tfb/maven:latest
ADD ./ /undertow-jersey
WORKDIR /undertow-jersey
RUN mvn clean package -P hikaricp
CMD java -jar target/undertow-jersey.jar
