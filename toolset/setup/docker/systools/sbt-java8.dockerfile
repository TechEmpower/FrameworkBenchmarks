FROM tfb/java8:latest

RUN wget https://dl.bintray.com/sbt/debian/sbt-1.1.1.deb
RUN apt install -yqq ./sbt-1.1.1.deb
