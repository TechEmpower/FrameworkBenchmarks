FROM tfb/act-base:latest
RUN mvn clean package
WORKDIR /act/target/dist
RUN unzip -q *.zip
CMD java \
    -server \
    -Djava.security.egd=file:/dev/./urandom \
    -Xms1G \
    -Xmx1G \
    -Xss320k \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -Dapp.mode=prod \
    -Dapp.nodeGroup= \
    -Dprofile=json_plaintext \
    -Dxio.worker_threads.max=256 \
    -cp "/act/target/dist/classes:/act/target/dist/lib/*" \
    com.techempower.act.AppEntry
