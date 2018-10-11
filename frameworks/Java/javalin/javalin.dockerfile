FROM gradle:4.7.0-jdk8
USER root
WORKDIR /javalin
COPY build.gradle build.gradle
COPY ./ ./
COPY src src
CMD ["gradle", "--no-daemon", "--refresh-dependencies", "run", "-Pargs=8080,production"]
