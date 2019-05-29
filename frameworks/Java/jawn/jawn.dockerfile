FROM gradle:5.4.1-jdk11 as gradle
USER root
WORKDIR /jawn
COPY build.gradle build.gradle
COPY src src
COPY webapp webapp
CMD ["gradle", "--no-daemon", "--refresh-dependencies", "run", "-Pargs=8080,production"]
