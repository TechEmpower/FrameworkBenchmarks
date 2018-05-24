FROM maven:3.5.3-jdk-10 as maven
WORKDIR /restexpress
COPY config config
COPY src src
COPY pom.xml pom.xml
COPY zip-with-dependencies.xml zip-with-dependencies.xml
RUN mvn package -q
RUN mvn assembly:single -q
WORKDIR target
RUN unzip -q world-1.0-SNAPSHOT-zip-with-dependencies.zip
WORKDIR world-1.0-SNAPSHOT
CMD ["java", "-jar", "world-1.0-SNAPSHOT.jar"]
