FROM ubuntu:18.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get -y install curl && \
    rm -rf /var/lib/apt/lists/*

ENV GRAALVM_VERSION=1.0.0-rc6

# Get GraalVM CE
RUN echo "Pulling graalvm ${GRAALVM_VERSION} binary from Github." \
    && curl -sSLf https://github.com/oracle/graal/releases/download/vm-${GRAALVM_VERSION}/graalvm-ce-${GRAALVM_VERSION}-linux-amd64.tar.gz > /tmp/graalvm-ce-${GRAALVM_VERSION}-linux-amd64.tar.gz \
    && mkdir -p /opt/java \
    && tar -zxf /tmp/graalvm-ce-${GRAALVM_VERSION}-linux-amd64.tar.gz -C /opt/java \
    && rm /tmp/graalvm-ce-${GRAALVM_VERSION}-linux-amd64.tar.gz

ENV GRAALVM_HOME=/opt/java/graalvm-ce-${GRAALVM_VERSION}
ENV JAVA_HOME=${GRAALVM_HOME}
ENV PATH=${PATH}:${JAVA_HOME}/bin

# Set working dir
RUN mkdir /app
WORKDIR /app

COPY ./ /app

# Get dependencies
RUN npm --unsafe-perm install
# Generate a runtime blog
RUN npm run package

CMD ${GRAALVM_HOME}/bin/java \
    -server                                           \
    -XX:+UseNUMA                                      \
    -XX:+UseParallelGC                                \
    -XX:+AggressiveOpts                               \
    -Dvertx.disableMetrics=true                       \
    -Dvertx.disableH2c=true                           \
    -Dvertx.disableWebsockets=true                    \
    -Dvertx.flashPolicyHandler=false                  \
    -Dvertx.threadChecks=false                        \
    -Dvertx.disableContextTimings=true                \
    -Dvertx.disableTCCL=true                          \
    -jar                                              \
    target/es4x-0.0.1-fat.jar                         \
    --instances                                       \
    `grep --count ^processor /proc/cpuinfo`