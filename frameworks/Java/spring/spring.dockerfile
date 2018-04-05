FROM techempower/maven:0.1

ADD ./ /spring
WORKDIR /spring
RUN mvn clean package
CMD java -jar target/spring.war
