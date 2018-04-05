FROM techempower/maven-java8:0.1 as maven

ADD ./ /spark
WORKDIR /spark
RUN mvn clean package

FROM techempower/resin-java8:0.1

COPY --from=maven /spark/target/spark.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
