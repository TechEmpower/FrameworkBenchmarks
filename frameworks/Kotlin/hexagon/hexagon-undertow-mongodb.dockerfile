FROM techempower/java:0.1

ADD ./ /hexagon
WORKDIR /hexagon
RUN ./gradlew -x test
ENV DBSTORE mongodb
ENV WEBENGINE undertow
CMD build/install/hexagon/bin/hexagon
