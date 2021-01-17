FROM gradle:5.4.1-jdk11 as gradle
WORKDIR /jawn
COPY build.gradle build.gradle
COPY src src
RUN gradle install --refresh-dependencies --no-daemon

FROM openjdk:11.0.5-jre-stretch
WORKDIR /jawn
COPY --from=gradle /jawn/build/install/jawn .
ENTRYPOINT ["bin/jawn"]

EXPOSE 8080

CMD ["8080","production"]
