FROM techempower/sbt:0.1

COPY ./ ./

RUN sbt stage

CMD ./target/universal/stage/bin/s-server-tfb -J-XX:+UseBiasedLocking -J-XX:+UseParallelGC -J-XX:+AggressiveOpts