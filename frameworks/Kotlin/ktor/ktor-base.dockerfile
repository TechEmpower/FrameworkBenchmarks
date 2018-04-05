FROM techempower/maven:0.1

ADD ./ /ktor
WORKDIR /ktor
RUN mvn clean package
