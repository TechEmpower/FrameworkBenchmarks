FROM techempower/java:0.1
FROM techempower/gradle:0.1

ADD ./ /hexagon
WORKDIR /hexagon
RUN gradle -x test
ENV DBSTORE postgresql
ENV WEBENGINE undertow
CMD build/install/hexagon/bin/hexagon
