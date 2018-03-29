FROM tfb/sbt:latest

ADD ./ /s_server
WORKDIR /s_server

RUN sbt stage

CMD ./target/universal/stage/bin/s-server-tfb \
    -J-XX:+UseBiasedLocking \
    -J-XX:+UseParallelGC \
    -J-XX:+AggressiveOpts
