FROM hseeberger/scala-sbt:8u181_2.12.6_1.2.1
WORKDIR /play2
COPY play2-scala .

RUN sed -i 's/.enablePlugins(PlayScala, PlayNettyServer)/.enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayAkkaHttpServer)/g' build.sbt

RUN sbt stage
CMD ["target/universal/stage/bin/play2-scala", "-Dplay.server.provider=play.core.server.NettyServerProvider", "-J-server", "-J-Xms1g", "-J-Xmx1g", "-J-XX:NewSize=512m", "-J-XX:+UseG1GC", "-J-XX:MaxGCPauseMillis=30", "-J-XX:-UseBiasedLocking", "-J-XX:+AlwaysPreTouch"]
