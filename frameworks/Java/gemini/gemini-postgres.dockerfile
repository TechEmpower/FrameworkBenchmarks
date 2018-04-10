FROM openjdk:9-jdk-slim as ant
RUN apt update -qqy && apt install -qqy ant

WORKDIR /gemini
COPY Docroot Docroot
COPY Source Source
COPY build.xml build.xml
COPY ivy.xml ivy.xml
COPY ivysettings.xml ivysettings.xml

RUN mv Docroot/WEB-INF/gemini-postgres.conf Docroot/WEB-INF/GeminiHello.conf
RUN mkdir Docroot/WEB-INF/classes
RUN mkdir Docroot/WEB-INF/lib
RUN ant resolve
RUN ant compile

FROM techempower/resin:0.1
COPY --from=ant /gemini /gemini
CMD java -jar ${RESIN_HOME}/lib/resin.jar -conf /gemini/Docroot/WEB-INF/resin.xml console
