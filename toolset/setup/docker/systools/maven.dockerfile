FROM techempower/java:0.1

RUN mkdir /maven
WORKDIR /maven
RUN curl -s http://mirrors.advancedhosters.com/apache/maven/maven-3/3.5.2/binaries/apache-maven-3.5.2-bin.tar.gz | tar xz
ENV MAVEN_HOME=/maven/apache-maven-3.5.2
ENV PATH="${MAVEN_HOME}/bin:${PATH}"

ADD maven-settings.xml /root/.m2/settings.xml
