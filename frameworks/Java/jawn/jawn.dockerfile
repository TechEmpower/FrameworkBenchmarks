FROM tfb/gradle-java8:latest
ADD ./ /jawn
WORKDIR /jawn
RUN gradle clean
CMD gradle --no-daemon --refresh-dependencies run -Pargs=8080,production
