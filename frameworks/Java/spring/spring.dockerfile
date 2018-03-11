FROM tfb/maven:latest
ADD ./ /spring
WORKDIR /spring
RUN mvn clean package
CMD java -jar target/spring.war
