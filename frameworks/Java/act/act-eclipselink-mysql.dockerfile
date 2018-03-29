FROM techempower/act-base:0.1

RUN mvn -Peclipselink_mysql clean package
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
    -Dprofile=eclipselink_mysql \
    -Dxio.worker_threads.max=256 \
    -Dmysql.host=tfb-database \
    -cp "/act/target/dist/classes:/act/target/dist/lib/*" \
    com.techempower.act.AppEntry
