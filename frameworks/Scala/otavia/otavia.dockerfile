FROM nightscape/scala-mill:eclipse-temurin-17.0.8.1_1-jdk-focal_0.11.6_3.3.0
WORKDIR /otavia
COPY benchmark benchmark
COPY build.sc build.sc
ENV COURSIER_REPOSITORIES=ivy2Local|central
RUN mill benchmark.assembly

EXPOSE 8080

CMD java -server \
    -Dcc.otavia.actor.worker.size=28 -Dcc.otavia.nio.worker.size=56 \
    -jar \
    out/benchmark/assembly.dest/out.jar \
    jdbc:postgresql://tfb-database:5432/hello_world \
    benchmarkdbuser benchmarkdbpass 56
