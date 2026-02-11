FROM container-registry.oracle.com/graalvm/native-image:25
RUN microdnf install findutils # Gradle 8.7 requires xargs
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN ./gradlew micronaut-data-r2dbc:nativeCompile -x test -x internalStartTestResourcesService --no-daemon

WORKDIR /micronaut
RUN mv /home/gradle/src/micronaut-data-r2dbc/build/native/nativeCompile/micronaut-data-r2dbc micronaut

EXPOSE 8080
ENV MICRONAUT_ENVIRONMENTS=benchmark
ENTRYPOINT "./micronaut"
