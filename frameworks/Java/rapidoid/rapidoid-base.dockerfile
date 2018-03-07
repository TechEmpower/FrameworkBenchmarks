FROM tfb/maven:latest
ADD ./ /rapidoid
WORKDIR /rapidoid
RUN mvn clean compile assembly:single
