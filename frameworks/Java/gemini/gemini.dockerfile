FROM techempower/ant:0.1 as ant

ADD Docroot/ /gemini/Docroot
ADD Source/ /gemini/Source
ADD build.xml /gemini/
ADD ivy.xml /gemini/
ADD ivysettings.xml /gemini/

RUN cd /gemini/Docroot/WEB-INF; mv gemini.conf GeminiHello.conf;

RUN cd /gemini; mkdir -p Docroot/WEB-INF/classes; mkdir -p Docroot/WEB-INF/lib; ant resolve; ant compile

FROM techempower/resin:0.1

COPY --from=ant /gemini /gemini

CMD java -jar ${RESIN_HOME}/lib/resin.jar -conf /gemini/Docroot/WEB-INF/resin.xml console