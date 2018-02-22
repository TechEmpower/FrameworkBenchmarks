FROM tfb:latest

RUN mkdir /java
WORKDIR /java
RUN curl https://download.java.net/java/GA/jdk9/9.0.4/binaries/openjdk-9.0.4_linux-x64_bin.tar.gz | tar xz
ENV JAVA_HOME=/java/jdk-9.0.4
ENV PATH="${JAVA_HOME}/bin:${PATH}"

RUN mkdir /maven
WORKDIR /maven
RUN curl http://mirrors.advancedhosters.com/apache/maven/maven-3/3.5.2/binaries/apache-maven-3.5.2-bin.tar.gz | tar xz
ENV MAVEN_HOME=/maven/apache-maven-3.5.2
ENV PATH="${MAVEN_HOME}/bin:${PATH}"
