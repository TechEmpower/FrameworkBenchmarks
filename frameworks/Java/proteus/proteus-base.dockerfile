FROM techempower/maven-java8:0.1

ADD ./ /proteus
WORKDIR /proteus
RUN mvn -U clean package
