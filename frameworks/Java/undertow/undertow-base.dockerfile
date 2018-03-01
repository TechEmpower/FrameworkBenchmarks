FROM tfb/maven:latest
ADD ./ /undertow
WORKDIR /undertow
RUN mvn clean package
