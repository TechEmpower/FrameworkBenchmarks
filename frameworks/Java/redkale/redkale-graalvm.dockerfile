FROM ghcr.io/graalvm/graalvm-ce:latest
WORKDIR /redkale
RUN apt-get install -y wget
RUN wget https://downloads.apache.org/maven/maven-3/3.8.1/binaries/apache-maven-3.8.1-bin.tar.gz
RUN tar xzvf apache-maven-3.8.1-bin.tar.gz
RUN cp -R apache-maven-3.8.1 /usr/local/bin
RUN export PATH=apache-maven-3.8.1/bin:$PATH
RUN export PATH=/usr/local/bin/apache-maven-3.8.1/bin:$PATH
RUN ln -s /usr/local/bin/apache-maven-3.8.1/bin/mvn /usr/local/bin/mvn

COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q

FROM ghcr.io/graalvm/graalvm-ce:latest
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:AutoBoxCacheMax=40000", "-Dbenchmarks.cache=true", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]
