FROM tfb/java:latest
ADD ./ /pronghorn
WORKDIR /pronghorn
CMD ./gradlew --no-daemon clean run
