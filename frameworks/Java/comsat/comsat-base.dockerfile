FROM tfb/java8:latest
ADD ./ /comsat
WORKDIR /comsat
RUN ./gradlew clean capsule
