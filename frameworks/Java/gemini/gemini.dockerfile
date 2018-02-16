FROM resin:latest

RUN apt-get install -qqy -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    ant

ADD Docroot/ /gemini/Docroot
ADD Source/ /gemini/Source
ADD build.xml /gemini/
ADD ivy.xml /gemini/
ADD ivysettings.xml /gemini/

RUN cd /gemini/Docroot/WEB-INF; mv gemini.conf GeminiHello.conf;

RUN cd /gemini; mkdir -p Docroot/WEB-INF/classes; mkdir -p Docroot/WEB-INF/lib; ant resolve; ant compile

EXPOSE 8080

CMD ["resinctl", "-conf", "/gemini/Docroot/WEB-INF/resin.xml", "console"]