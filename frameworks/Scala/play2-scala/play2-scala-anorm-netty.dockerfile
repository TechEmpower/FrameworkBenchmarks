FROM hseeberger/scala-sbt:8u265_1.3.13_2.13.3
WORKDIR /play2
COPY play2-scala-anorm .

RUN sed -i 's/.enablePlugins(PlayScala, PlayNettyServer)/.enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayAkkaHttpServer)/g' build.sbt

RUN sbt stage

EXPOSE 9000

CMD target/universal/stage/bin/play2-scala-anorm -Dplay.server.provider=play.core.server.NettyServerProvider -J-server -J-Xms1g -J-Xmx1g -J-XX:NewSize=512m -J-XX:+UseG1GC -J-XX:MaxGCPauseMillis=30 -J-XX:-UseBiasedLocking -J-XX:+AlwaysPreTouch -Dthread_count=$(nproc) -Dphysical_cpu_count=$(grep ^cpu\\scores /proc/cpuinfo | uniq | awk '{print $4}') -Ddb_pool_size=$(( $(( $(nproc) * 2 )) + 1 ))
