FROM maven:3.9.9-eclipse-temurin-24-noble as maven
WORKDIR /netty
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM maven:3.9.9-eclipse-temurin-24-noble
WORKDIR /netty
COPY --from=maven /netty/target/app.jar app.jar
COPY run_netty.sh run_netty.sh

EXPOSE 8080
# see https://github.com/netty/netty/issues/14942
# remember to run this with --privileged since https://github.com/TechEmpower/FrameworkBenchmarks/blob/c94f7f95bd751f86a57dea8b63fb8f336bdbbde3/toolset/utils/docker_helper.py#L239 does it
ENTRYPOINT "./run_netty.sh"