FROM techempower/java8:0.1

RUN mkdir /sbt
WORKDIR /sbt
RUN curl -sL https://github.com/sbt/sbt/releases/download/v1.1.2/sbt-1.1.2.tgz | tar xz
ENV SBT_HOME /sbt/sbt
ENV PATH ${SBT_HOME}/bin:${PATH}
