FROM undertow-base:latest
CMD java -jar target/hello-undertow.jar NO_DATABASE
