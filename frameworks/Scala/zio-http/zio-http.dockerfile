FROM alpine as builder

# Install
RUN apk add -U openjdk11
ENV SCALA_VERSION=2.13.3 \
    SCALA_HOME=/usr/share/scala

# NOTE: bash is used by scala/scalac scripts, and it cannot be easily replaced with ash.
RUN apk add --no-cache --virtual=.build-dependencies wget ca-certificates && \
    apk add --no-cache bash curl jq && \
    cd "/tmp" && \
    wget --no-verbose "https://downloads.typesafe.com/scala/${SCALA_VERSION}/scala-${SCALA_VERSION}.tgz" && \
    tar xzf "scala-${SCALA_VERSION}.tgz" && \
    mkdir "${SCALA_HOME}" && \
    rm "/tmp/scala-${SCALA_VERSION}/bin/"*.bat && \
    mv "/tmp/scala-${SCALA_VERSION}/bin" "/tmp/scala-${SCALA_VERSION}/lib" "${SCALA_HOME}" && \
    ln -s "${SCALA_HOME}/bin/"* "/usr/bin/" && \
    apk del .build-dependencies && \
    rm -rf "/tmp/"*

RUN export PATH="/usr/local/sbt/bin:$PATH" &&  apk update && apk add ca-certificates wget tar && mkdir -p "/usr/local/sbt" && wget -qO - --no-check-certificate "https://github.com/sbt/sbt/releases/download/v1.4.7/sbt-1.4.7.tgz" | tar xz -C /usr/local/sbt --strip-components=1 && sbt sbtVersion -Dsbt.rootdir=true
COPY . /app
WORKDIR /app
RUN addgroup -S app && adduser -S app -s /bin/false -G app
RUN ["chown", "-R", "app:app", "/app"]
RUN /usr/local/sbt/bin/sbt assembly
RUN ls /app

FROM adoptopenjdk/openjdk11
COPY --from=builder /app/target/scala-2.13/zio-http-assembly-1.0.0.jar /app/zio-http-assembly-1.0.0.jar
ENTRYPOINT ["java",\
"-jar",\
"-Xms2G",\
"-Xmx2G",\
"-Xss2m",\
"/app/zio-http-assembly-1.0.0.jar"]