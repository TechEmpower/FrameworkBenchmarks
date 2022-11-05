FROM eclipse-temurin:19-jdk-alpine as gradle

CMD ["gradle"]

ENV GRADLE_HOME /opt/gradle

RUN set -o errexit -o nounset \
    && echo "Adding gradle user and group" \
    && addgroup --system --gid 1000 gradle \
    && adduser --system --ingroup gradle --uid 1000 --shell /bin/ash gradle \
    && mkdir /home/gradle/.gradle \
    && chown -R gradle:gradle /home/gradle \
    \
    && echo "Symlinking root Gradle cache to gradle Gradle cache" \
    && ln -s /home/gradle/.gradle /root/.gradle

VOLUME /home/gradle/.gradle

WORKDIR /home/gradle

RUN set -o errexit -o nounset \
    && echo "Installing VCSes" \
    && apk add --no-cache \
      git \
      git-lfs \
      mercurial \
      subversion \
    \
    && echo "Testing VCSes" \
    && which git \
    && which git-lfs \
    && which hg \
    && which svn

ENV GRADLE_VERSION 7.6-milestone-1
ARG GRADLE_DOWNLOAD_SHA256=f6b8596b10cce501591e92f229816aa4046424f3b24d771751b06779d58c8ec4
RUN set -o errexit -o nounset \
    && wget --no-verbose --output-document=gradle.zip "https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip" \
    && unzip gradle.zip \
    && rm gradle.zip \
    && mv "gradle-${GRADLE_VERSION}" "${GRADLE_HOME}/" \
    && ln -s "${GRADLE_HOME}/bin/gradle" /usr/bin/gradle \
    && gradle --version

USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY core core
COPY jettyloom jettyloom
RUN gradle --quiet jettyloom:shadowJar

FROM amazoncorretto:19 as java
COPY --from=gradle /http4k/jettyloom/build/libs/http4k-jettyloom-benchmark.jar /home/app/http4k-jettyloon/

WORKDIR /home/app/http4k-jettyloom

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "--enable-preview", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "jettyloom/build/libs/http4k-jettyloom-benchmark.jar"]
