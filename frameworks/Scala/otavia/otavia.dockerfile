FROM nightscape/scala-mill:eclipse-temurin-21.0.6_7-jdk-jammy_0.12.10
WORKDIR /otavia
COPY benchmark benchmark
COPY build.sc build.sc
COPY .mill-version .mill-version
ENV COURSIER_REPOSITORIES=ivy2Local|central
RUN mill benchmark.assembly

EXPOSE 8080

CMD java -server \
    -Dcc.otavia.actor.worker.size=56 \
    -Dcc.otavia.buffer.page.size=8 \
    -Dio.netty5.noKeySetOptimization=true \
    -jar \
    out/benchmark/assembly.dest/out.jar \
    jdbc:postgresql://tfb-database:5432/hello_world \
    benchmarkdbuser benchmarkdbpass 56
