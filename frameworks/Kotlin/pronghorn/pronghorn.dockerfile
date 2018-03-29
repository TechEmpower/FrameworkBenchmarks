FROM tfb/gradle:latest
ADD ./ /pronghorn
WORKDIR /pronghorn
CMD gradle --no-daemon clean run
