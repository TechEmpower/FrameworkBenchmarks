FROM maven:3.6.1-jdk-11-slim as maven

WORKDIR /gemini

COPY firenio/src src
COPY firenio/pom.xml pom.xml

RUN mvn -q clean install

WORKDIR target

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dlite=false", "-Dcore=1", "-Dframe=16", "-DreadBuf=512", "-Dpool=true", "-Ddirect=true", "-Dinline=true", "-Dlevel=1", "-Dread=false", "-Depoll=true", "-Dnodelay=true", "-Dcachedurl=false", "-DunsafeBuf=true", "-jar", "GhApplication-0.0.1.jar"]
