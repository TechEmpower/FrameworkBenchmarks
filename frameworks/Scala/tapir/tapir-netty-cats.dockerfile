FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_3.6.4

WORKDIR /tapir-benchmark
COPY netty-cats-server netty-cats-server
COPY common common
COPY project project
COPY build.sbt build.sbt
RUN sbt netty-cats-server/assembly

EXPOSE 8080
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-Dio.netty.leakDetection.level=disabled", "-Dio.netty.recycler.maxCapacityPerThread=0", "-jar", "/tapir-benchmark/netty-cats-server/target/scala-3.6.4/tapir-netty-cats-server-assembly-1.0.0.jar"]