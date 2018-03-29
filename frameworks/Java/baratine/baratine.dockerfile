FROM techempower/maven:0.1

ADD ./ /baratine
WORKDIR /baratine
RUN mvn clean package
CMD java -jar target/testTechempowerBaratine-0.0.1-SNAPSHOT.jar tfb-database
