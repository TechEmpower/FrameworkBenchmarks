export ACTIVE_ENV=local

mvn clean package

cp -f target/activeweb.war ~/programs/apache-tomcat-7.0.30_framework_benchmark/webapps/

