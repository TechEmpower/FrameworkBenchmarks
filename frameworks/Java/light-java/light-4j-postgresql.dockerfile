FROM techempower/maven:0.1

ADD ./ /light-4j
WORKDIR /light-4j
RUN mvn clean package
CMD java \
    -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005 \
    -server \
    -Xms512m \
    -Xmx2g \
    -jar target/techempower-1.0.0.jar
