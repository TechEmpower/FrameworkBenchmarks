FROM tfb/gradle-java8:latest
ADD ./ /comsat
WORKDIR /comsat
RUN gradle clean capsule
