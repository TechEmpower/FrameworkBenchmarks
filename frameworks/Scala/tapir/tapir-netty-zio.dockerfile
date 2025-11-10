FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_3.6.4

WORKDIR /tapir-benchmark
COPY netty-zio-server netty-zio-server
COPY common common
COPY project project
COPY build.sbt build.sbt
RUN sbt netty-zio-server/assembly

EXPOSE 8080
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-Dio.netty.leakDetection.level=disabled", "-Dio.netty.recycler.maxCapacityPerThread=0", "-jar", "/tapir-benchmark/netty-zio-server/target/scala-3.6.4/tapir-netty-zio-server-assembly-1.0.0.jar"]