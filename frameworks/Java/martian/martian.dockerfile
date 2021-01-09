FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /martian
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /martian
#将当前client.jar 复制到容器根目录下
ADD --from=maven /martian/target/martian.jar martian.jar
#将依赖包 复制到容器根目录/libs下
ADD --from=maven /martian/target/lib /lib

EXPOSE 8080

CMD java -jar martian.jar &