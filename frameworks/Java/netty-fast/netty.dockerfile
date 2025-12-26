FROM maven:3.9-eclipse-temurin-25-noble as maven
WORKDIR /netty-fast
COPY pom.xml pom.xml
COPY src src
RUN mvn -q -DskipTests package

FROM eclipse-temurin:25-jre-noble
WORKDIR /netty-fast
COPY --from=maven /netty-fast/target/app.jar ./app.jar
COPY run_netty.sh ./run_netty.sh
RUN chmod +x ./run_netty.sh
EXPOSE 8080
ENTRYPOINT ["./run_netty.sh"]
