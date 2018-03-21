FROM tfb/ant:latest as ant

RUN apt-get install -qqy -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    ant

ADD Docroot/ /gemini/Docroot
ADD Source/ /gemini/Source
ADD build.xml /gemini/
ADD ivy.xml /gemini/
ADD ivysettings.xml /gemini/

RUN cd /gemini/Docroot/WEB-INF; mv gemini-mysql.conf GeminiHello.conf;

RUN cd /gemini; mkdir -p Docroot/WEB-INF/classes; mkdir -p Docroot/WEB-INF/lib; ant resolve; ant compile

FROM tfb/resin:latest

COPY --from=ant /gemini /gemini

CMD java -jar ${RESIN_HOME}/lib/resin.jar -conf /gemini/Docroot/WEB-INF/resin.xml console