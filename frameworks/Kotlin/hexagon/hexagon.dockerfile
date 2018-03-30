FROM techempower/java:0.1

ADD ./ /hexagon
WORKDIR /hexagon
RUN gradle -x test
ENV DBSTORE mongodb
ENV WEBENGINE jetty
CMD build/install/hexagon/bin/hexagon
