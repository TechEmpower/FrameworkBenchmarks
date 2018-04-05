FROM techempower/maven:0.1
ADD ./ /wildfly
WORKDIR /wildfly
RUN mvn clean initialize package -Pbenchmark
ENV JAVA_OPTS="-Djava.net.preferIPv4Stack=true -Xms2g -Xmx2g -XX:+UseG1GC -XX:MaxGCPauseMillis=50"
CMD target/wildfly-12.0.0.Final/bin/standalone.sh -b 0.0.0.0
