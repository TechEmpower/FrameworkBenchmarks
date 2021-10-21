FROM ghcr.io/graalvm/graalvm-ce:java11-21.3.0
RUN gu install native-image
WORKDIR /micronaut
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY gradlew gradlew
COPY gradle gradle
COPY src src
RUN ./gradlew build nativeImage --no-daemon

EXPOSE 8080

CMD ["/micronaut/build/native-image/application", "-Dmicronaut.environments=benchmark", "-Dlog-root-level=OFF"]