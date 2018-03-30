FROM techempower/java:0.1

ADD ./ /hexagon
WORKDIR /hexagon
RUN gradle -x test
ENV DBSTORE postgresql
ENV WEBENGINE jetty
CMD build/install/hexagon/bin/hexagon
