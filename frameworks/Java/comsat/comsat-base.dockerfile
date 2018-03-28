FROM techempower/java8:0.1

ADD ./ /comsat
WORKDIR /comsat
RUN ./gradlew clean capsule
