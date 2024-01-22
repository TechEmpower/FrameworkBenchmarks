FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.1_12_1.9.8_2.13.12
WORKDIR /play2
COPY play2-scala-slick .

RUN sed -i 's/.enablePlugins(PlayScala, PlayNettyServer)/.enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayAkkaHttpServer)/g' build.sbt

RUN sbt stage

EXPOSE 9000

CMD target/universal/stage/bin/play2-scala-slick -Dplay.server.provider=play.core.server.NettyServerProvider -J-server -J-Xms1g -J-Xmx1g -J-XX:NewSize=512m -J-XX:+UseG1GC -J-XX:MaxGCPauseMillis=30 -J-XX:+AlwaysPreTouch -Dthread_count=$(nproc) -Dphysical_cpu_count=$(grep ^cpu\\scores /proc/cpuinfo | uniq | awk '{print $4}') -Ddb_pool_size=$(( $(( $(nproc) * 2 )) + 1 ))
