FROM tfb/rapidoid-base:latest
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -cp target/rapidoid-1.0-jar-with-dependencies.jar \
    highlevel.Main \
    profiles=production
