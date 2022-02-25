FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /youi
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt pack
WORKDIR /youi/target/pack

EXPOSE 8080

CMD ["bin/main"]