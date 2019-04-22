FROM maven:3.5.3-jdk-10 as maven
WORKDIR /restexpress
COPY config config
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q
CMD ["java", "-jar", "world-1.0-SNAPSHOT.jar"]
