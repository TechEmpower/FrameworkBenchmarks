FROM ringo-base:latest

RUN ringo-admin install oberhamsi/sql-ringojs-client
RUN ringo-admin install orfon/reinhardt
RUN (cd ringo-${RINGO_VEFRSION}/packages/sql-ringojs-client/jars && curl -s -o mysql.jar https://repo1.maven.org/maven2/mysql/mysql-connector-java/5.1.39/mysql-connector-java-5.1.39.jar)

CMD ["ringo", "--production", "-J-server", "-J-Xmx1g", "-J-Xms1g", "ringo-main.js", "--host 0.0.0.0"]
