FROM openjdk:8-jdk

WORKDIR /grails
COPY grails-app grails-app
COPY web-app web-app
COPY application.properties application.properties

ENV GRAILS_VERSION 2.4.4
ENV GRAILS_HOME /grails/grails-${GRAILS_VERSION}
ENV GRAILS_WORK_DIR ${GRAILS_HOME}/.grails
ENV GRAILS_AGENT_CACHE_DIR ${GRAILS_WORK_DIR}/.slcache

RUN curl -sL -O https://github.com/grails/grails-core/releases/download/v${GRAILS_VERSION}/grails-${GRAILS_VERSION}.zip
RUN unzip -q grails-${GRAILS_VERSION}.zip
ENV PATH ${GRAILS_HOME}/bin:${PATH}

RUN grails -Dgrails.work.dir=${GRAILS_WORK_DIR} -non-interactive -plain-output refresh-dependencies
RUN grails -Dgrails.work.dir=${GRAILS_WORK_DIR} -non-interactive -plain-output compile
RUN grails -Dgrails.work.dir=${GRAILS_WORK_DIR} prod -non-interactive -plain-output war

WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.61.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
RUN cp /grails/target/hello-0.1.war webapps/ROOT.war
COPY resin.xml conf/resin.xml

EXPOSE 8080

CMD ["java", "-jar", "lib/resin.jar", "console"]
