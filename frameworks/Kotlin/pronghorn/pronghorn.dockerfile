FROM techempower/java:0.1

ADD ./ /pronghorn
WORKDIR /pronghorn
CMD gradle --no-daemon clean run
