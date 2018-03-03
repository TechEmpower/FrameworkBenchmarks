FROM tfb/maven:latest
ADD ./ /ktor
WORKDIR /ktor
RUN ./mvnw clean package
