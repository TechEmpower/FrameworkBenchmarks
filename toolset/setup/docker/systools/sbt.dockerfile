FROM techempower/java:0.1

RUN wget -nv https://dl.bintray.com/sbt/debian/sbt-1.1.1.deb
RUN apt install -yqq ./sbt-1.1.1.deb
