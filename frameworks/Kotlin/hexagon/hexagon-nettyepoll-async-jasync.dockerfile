#
# BUILD
#
FROM docker.io/gradle:8.1.1-jdk17-alpine AS build
USER root
WORKDIR /hexagon

ADD . .
RUN gradle --quiet classes
RUN gradle --quiet -x test installDist

#
# RUNTIME
#
FROM docker.io/eclipse-temurin:17-jre-alpine
<<<<<<<< HEAD:frameworks/Kotlin/hexagon/hexagon-nettyepoll-async-jasync.dockerfile
ARG PROJECT=hexagon_nettyepoll_async_jasync
========
ARG PROJECT=hexagon_nettyepoll_async_pgclient
>>>>>>>> 1d0e4006abc9d1d1cfee1f3f2e5bdbc9fbab8f11:frameworks/Kotlin/hexagon/hexagon-nettyepoll-async-pgclient.dockerfile

ENV POSTGRESQL_DB_HOST tfb-database
ENV JDK_JAVA_OPTIONS -XX:+AlwaysPreTouch -XX:+UseParallelGC -XX:+UseNUMA

COPY --from=build /hexagon/$PROJECT/build/install/$PROJECT /opt/$PROJECT

<<<<<<<< HEAD:frameworks/Kotlin/hexagon/hexagon-nettyepoll-async-jasync.dockerfile
ENTRYPOINT [ "/opt/hexagon_nettyepoll_async_jasync/bin/hexagon_nettyepoll_async_jasync" ]
========
ENTRYPOINT [ "/opt/hexagon_nettyepoll_async_pgclient/bin/hexagon_nettyepoll_async_pgclient" ]
>>>>>>>> 1d0e4006abc9d1d1cfee1f3f2e5bdbc9fbab8f11:frameworks/Kotlin/hexagon/hexagon-nettyepoll-async-pgclient.dockerfile
