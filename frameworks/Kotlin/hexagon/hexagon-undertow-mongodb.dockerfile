FROM tfb/java:latest
ADD ./ /hexagon
WORKDIR /hexagon
RUN ./gradlew -x test
ENV DBSTORE mongodb
ENV WEBENGINE undertow
CMD build/install/hexagon/bin/hexagon
