FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.7_3.6.3

WORKDIR /kyo-tapir
COPY src src
COPY project project
COPY build.sbt build.sbt
RUN sbt assembly

EXPOSE 9999
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-Dio.netty.leakDetection.level=disabled", "-Dio.netty.recycler.maxCapacityPerThread=0", "-jar", "/kyo-tapir/target/scala-3.6.3/kyo-tapir-assembly-1.0.0.jar"]