FROM techempower/java8:0.1

RUN mkdir /gradle
WORKDIR /gradle
RUN curl -sL -O https://services.gradle.org/distributions/gradle-4.6-bin.zip
RUN unzip gradle-4.6-bin.zip
ENV GRADLE_HOME /gradle/gradle-4.6
ENV PATH ${GRADLE_HOME}/bin:${PATH}
