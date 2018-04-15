FROM techempower/wizzardo-http-base:0.1

CMD java \
    -Xmx2G \
    -Xms2G \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -jar build/libs/techempower-benchmark-all-1.0-SNAPSHOT.jar env=prod profiles.active=plain
