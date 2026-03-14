FROM container-registry.oracle.com/graalvm/native-image:25
RUN microdnf install findutils # Gradle 8.7 requires xargs
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN ./gradlew micronaut-r2dbc:nativeCompile -x test --no-daemon

WORKDIR /micronaut
RUN mv /home/gradle/src/micronaut-r2dbc/build/native/nativeCompile/micronaut-r2dbc micronaut

EXPOSE 8080
ENV MICRONAUT_ENVIRONMENTS=benchmark
ENTRYPOINT "./micronaut"
