FROM tfb/base:latest

RUN mkdir /java
WORKDIR /java
RUN curl -sL https://download.java.net/java/GA/jdk10/10/binaries/openjdk-10_linux-x64_bin.tar.gz | tar xz
ENV JAVA_HOME=/java/jdk-10
ENV PATH="${JAVA_HOME}/bin:${PATH}"
