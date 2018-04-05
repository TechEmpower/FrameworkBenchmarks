FROM techempower/base:0.1

RUN mkdir /java
WORKDIR /java
RUN curl -sL https://download.java.net/java/GA/jdk9/9.0.4/binaries/openjdk-9.0.4_linux-x64_bin.tar.gz | tar xz
ENV JAVA_HOME=/java/jdk-9.0.4
ENV PATH="${JAVA_HOME}/bin:${PATH}"
