@ECHO OFF

call mvn clean package

call java -DAPP_HOME=./ -jar target/redkale-benchmark-1.0.0.jar 
