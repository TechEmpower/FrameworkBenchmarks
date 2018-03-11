FROM tfb/maven-java8:latest
ADD ./ /proteus
WORKDIR /proteus
RUN mvn -U clean package
