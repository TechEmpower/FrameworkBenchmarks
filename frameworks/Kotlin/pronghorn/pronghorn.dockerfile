FROM techempower/java:0.1

ADD ./ /pronghorn
WORKDIR /pronghorn
CMD ./gradlew --no-daemon clean run
