FROM maven:3.9.11-amazoncorretto-21 as maven
WORKDIR /edap-http
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single

FROM amazoncorretto:21.0.8
WORKDIR /edap-http
COPY --from=maven /edap-http/target/edap-http-benchmark-1.0-SNAPSHOT-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "--add-exports", "java.base/sun.security.action=ALL-UNNAMED", "--add-exports", "java.naming/com.sun.jndi.ldap=ALL-UNNAMED", "--add-exports", "java.naming/com.sun.jndi.url.ldap=ALL-UNNAMED", "--add-exports", "jdk.naming.dns/com.sun.jndi.dns=ALL-UNNAMED", "--add-exports", "jdk.naming.dns/com.sun.jndi.url.dns=ALL-UNNAMED", "--add-exports", "java.security.jgss/sun.security.krb5.internal=ALL-UNNAMED", "--add-exports", "jdk.attach/sun.tools.attach=ALL-UNNAMED", "--add-opens", "java.base/java.util=ALL-UNNAMED", "--add-opens", "java.base/java.lang=ALL-UNNAMED", "--add-opens", "java.base/java.util.concurrent=ALL-UNNAMED", "--add-opens", "java.base/java.io=ALL-UNNAMED", "--add-opens", "java.base/java.nio=ALL-UNNAMED", "--add-opens", "java.base/sun.nio.ch=ALL-UNNAMED", "--add-opens", "java.naming/javax.naming.spi=ALL-UNNAMED", "--add-opens", "java.naming/com.sun.naming.internal=ALL-UNNAMED", "--add-opens", "jdk.naming.rmi/com.sun.jndi.url.rmi=ALL-UNNAMED", "--add-opens", "java.naming/javax.naming=ALL-UNNAMED", "--add-opens", "java.rmi/java.rmi=ALL-UNNAMED", "--add-opens", "java.sql/java.sql=ALL-UNNAMED", "--add-opens", "java.management/javax.management=ALL-UNNAMED", "--add-opens", "java.base/java.lang.reflect=ALL-UNNAMED", "--add-opens", "java.desktop/java.awt.image=ALL-UNNAMED", "--add-opens", "java.base/java.security=ALL-UNNAMED", "--add-opens", "java.base/java.net=ALL-UNNAMED", "--add-opens", "java.base/java.text=ALL-UNNAMED", "--add-opens", "java.base/sun.net.www.protocol.https=ALL-UNNAMED", "--add-exports", "jdk.management.agent/jdk.internal.agent=ALL-UNNAMED", "--add-exports", "java.base/jdk.internal.vm=ALL-UNNAMED", "--add-opens", "java.base/java.lang.ref=ALL-UNNAMED", "--add-opens=java.rmi/sun.rmi.transport=ALL-UNNAMED", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-cp", "app.jar", "io.edap.http.Bootstrap"]
