FROM techempower/maven:0.1

ADD ./ /bayou
WORKDIR /bayou
RUN mvn clean compile assembly:single
CMD java -jar target/bayou_TFB-0.1-jar-with-dependencies.jar
