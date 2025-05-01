FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_3.6.4

WORKDIR /kyo-scheduler-benchmark
COPY zio-http zio-http
COPY project project
COPY build.sbt build.sbt
RUN sbt zio-http/assembly

EXPOSE 8080
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-Dio.netty.leakDetection.level=disabled", "-Dio.netty.recycler.maxCapacityPerThread=0", "-jar", "/kyo-scheduler-benchmark/zio-http/target/scala-3.6.4/zio-http-kyo-scheduler-benchmark-assembly-1.0.0.jar"]