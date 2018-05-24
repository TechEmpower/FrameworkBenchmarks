FROM gradle:4.7.0-jdk10
USER root
WORKDIR /pronghorn
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY src src
CMD ["gradle", "--no-daemon", "run"]
