FROM tfb/maven:latest
ADD ./ /baratine
WORKDIR /baratine
RUN mvn clean package
CMD java -jar target/testTechempowerBaratine-0.0.1-SNAPSHOT.jar TFB-database
