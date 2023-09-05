FROM maven:3.8.3-openjdk-17 as maven
WORKDIR /lighty
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single

EXPOSE 8080

CMD java                       \
    -server                    \
    -jar /lighty/target/lighty-tester-0.0.1-jar-with-dependencies.jar
