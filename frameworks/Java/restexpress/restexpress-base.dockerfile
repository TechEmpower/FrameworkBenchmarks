FROM techempower/maven:0.1

ADD ./ /restexpress
WORKDIR /restexpress
RUN mvn clean package
RUN mvn assembly:single
WORKDIR /restexpress/target
RUN unzip world-1.0-SNAPSHOT-zip-with-dependencies.zip
WORKDIR /restexpress/target/world-1.0-SNAPSHOT
CMD java -jar world-1.0-SNAPSHOT.jar

