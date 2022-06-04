FROM clojure:openjdk-17-lein-2.9.8
WORKDIR /aleph
COPY src src
COPY project.clj project.clj
RUN lein uberjar

# HTTP server
EXPOSE 8080
# async-profiler HTTP-server
EXPOSE 8081
# JMX port
EXPOSE 9999

RUN apt update -y
RUN apt install perl -y

CMD ["java", "-server", "-Xms2G", "-Xmx2G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dvertx.disableMetrics=true", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-Dvertx.disableTCCL=true", "-Djava.net.preferIPv4Stack=true", "-jar", "target/hello-aleph-standalone.jar"]

# To enable JMX and async-profiler
#CMD ["java", "-XX:+UnlockDiagnosticVMOptions", "-XX:+DebugNonSafepoints", "-Djdk.attach.allowAttachSelf", "-Dcom.sun.management.jmxremote=true", "-Djava.rmi.server.hostname=0.0.0.0","-Dcom.sun.management.jmxremote.rmi.port=9999" ,"-Dcom.sun.management.jmxremote.port=9999", "-Dcom.sun.management.jmxremote.ssl=false", "-Dcom.sun.management.jmxremote.authenticate=false", "-server", "-Xms2G", "-Xmx2G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dvertx.disableMetrics=true", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-Dvertx.disableTCCL=true", "-Djava.net.preferIPv4Stack=true", "-jar", "target/hello-aleph-standalone.jar"]
