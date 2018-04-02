FROM techempower/sbt:0.1

ADD ./play2-scala /play_app
WORKDIR /play_app

RUN sbt stage

CMD target/universal/stage/bin/play2-scala \
    -Dplay.server.provider=play.core.server.NettyServerProvider \
    -J-server \
    -J-Xms1g \
    -J-Xmx1g \
    -J-XX:NewSize=512m \
    -J-XX:+UseG1GC \
    -J-XX:MaxGCPauseMillis=30 \
    -J-XX:-UseBiasedLocking \
    -J-XX:+AlwaysPreTouch
