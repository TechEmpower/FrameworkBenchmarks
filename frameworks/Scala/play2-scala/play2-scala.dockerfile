FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /play2
COPY play2-scala .
RUN sbt stage
CMD ["target/universal/stage/bin/play2-scala", "-Dplay.server.provider=play.core.server.AkkaHttpServerProvider", "-J-server", "-J-Xms1g", "-J-Xmx1g", "-J-XX:NewSize=512m", "-J-XX:+UseG1GC", "-J-XX:MaxGCPauseMillis=30", "-J-XX:-UseBiasedLocking", "-J-XX:+AlwaysPreTouch"]
