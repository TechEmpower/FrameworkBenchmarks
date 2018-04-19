FROM openjdk:10-jdk-slim
RUN apt update -qqy && apt install -yqq ant curl

WORKDIR /gemini
COPY Docroot Docroot
COPY Source Source
COPY build.xml build.xml
COPY ivy.xml ivy.xml
COPY ivysettings.xml ivysettings.xml

RUN mv Docroot/WEB-INF/gemini-mysql.conf Docroot/WEB-INF/GeminiHello.conf
RUN mkdir Docroot/WEB-INF/classes
RUN mkdir Docroot/WEB-INF/lib
RUN ant resolve
RUN ant package

WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.56.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
RUN cp /gemini/gemini.war webapps/ROOT.war
CMD ["java", "-jar", "lib/resin.jar", "console"]
