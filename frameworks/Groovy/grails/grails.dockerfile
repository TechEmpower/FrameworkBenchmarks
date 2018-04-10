FROM techempower/resin-java8:0.1
WORKDIR /grails
COPY grails-app grails-app
COPY web-app web-app
COPY application.properties application.properties

ENV GRAILS_VERSION 2.4.4
ENV GRAILS_HOME /grails/grails-${GRAILS_VERSION}
ENV GRAILS_WORK_DIR ${GRAILS_HOME}/.grails
ENV GRAILS_AGENT_CACHE_DIR ${GRAILS_WORK_DIR}/.slcache

RUN curl -sL -O http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-${GRAILS_VERSION}.zip
RUN unzip -q grails-${GRAILS_VERSION}.zip
ENV PATH ${GRAILS_HOME}/bin:${PATH}

RUN grails -Dgrails.work.dir=${GRAILS_WORK_DIR} -non-interactive -plain-output refresh-dependencies
RUN grails -Dgrails.work.dir=${GRAILS_WORK_DIR} -non-interactive -plain-output compile
RUN grails -Dgrails.work.dir=${GRAILS_WORK_DIR} prod -non-interactive -plain-output war

RUN cp target/hello-0.1.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
