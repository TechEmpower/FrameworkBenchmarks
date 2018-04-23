FROM gradle:4.6.0-jdk8
USER root
WORKDIR /jawn
COPY build.gradle build.gradle
COPY src src
COPY webapp webapp
CMD ["gradle", "--no-daemon", "--refresh-dependencies", "run", "-Pargs=8080,production"]
