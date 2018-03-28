FROM tfb/maven:latest
ADD ./ /ktor
WORKDIR /ktor
RUN mvn clean package
