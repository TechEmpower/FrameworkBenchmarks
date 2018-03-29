FROM tfb/gradle:latest
ADD ./ /hexagon
WORKDIR /hexagon
RUN gradle -x test
ENV DBSTORE postgresql
ENV WEBENGINE jetty
CMD build/install/hexagon/bin/hexagon
