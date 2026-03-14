FROM container-registry.oracle.com/graalvm/native-image:25
RUN microdnf install findutils # Gradle 8.7 requires xargs
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN ./gradlew micronaut-jdbc:nativeCompile -x test -x internalStartTestResourcesService --no-daemon

WORKDIR /micronaut
RUN mv /home/gradle/src/micronaut-jdbc/build/native/nativeCompile/micronaut-jdbc micronaut

EXPOSE 8080
ENV MICRONAUT_ENVIRONMENTS=benchmark
ENTRYPOINT "./micronaut"
