FROM techempower/maven:0.1

ADD ./ /undertow
WORKDIR /undertow
RUN mvn clean package
