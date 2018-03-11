FROM tfb/rapidoid-base:latest
CMD java \
    -server \
    -Xms512m \
    -Xmx2g \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -cp target/rapidoid-1.0-jar-with-dependencies.jar \
    highlevel.Main \
    profiles=postgres,production
