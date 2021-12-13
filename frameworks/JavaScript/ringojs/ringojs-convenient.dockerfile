FROM openjdk:8-jdk

WORKDIR /ringojs_framework
ENV RINGOJS_VERSION 1.1.0
RUN curl -sL -O https://github.com/ringo/ringojs/releases/download/v${RINGOJS_VERSION}/ringojs-${RINGOJS_VERSION}.tar.gz
RUN tar xf ringojs-${RINGOJS_VERSION}.tar.gz
ENV RINGOJS_HOME /ringojs_framework/ringojs-${RINGOJS_VERSION}
ENV PATH ${RINGOJS_HOME}/bin:${PATH}

WORKDIR /ringojs_app
COPY app app
COPY config config
COPY templates templates
COPY ringo-convenient-main.js ringo-convenient-main.js

RUN ringo-admin install grob/ringo-sqlstore
RUN ringo-admin install ringo/stick
RUN ringo-admin install orfon/reinhardt
RUN curl -sL -o ${RINGOJS_HOME}/packages/ringo-sqlstore/jars/mysql.jar https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.18/mysql-connector-java-8.0.18.jar

EXPOSE 8080

CMD ["ringo", "--production", "-J-server", "-J-Xmx1g", "-J-Xms1g", "ringo-convenient-main.js", "--host", "0.0.0.0"]
