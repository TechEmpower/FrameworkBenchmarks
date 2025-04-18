FROM nightscape/scala-mill:eclipse-temurin-21.0.6_7-jdk-jammy_0.12.10
WORKDIR /sharaf

COPY src src
COPY build.mill build.mill
COPY mill mill
RUN chmod 777 mill
COPY .mill-version .mill-version

RUN ./mill assembly

EXPOSE 8080

CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-jar", "out/assembly.dest/out.jar"]
