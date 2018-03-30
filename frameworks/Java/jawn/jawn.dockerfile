FROM techempower/java8:0.1

ADD ./ /jawn
WORKDIR /jawn
RUN gradle clean
CMD gradle --no-daemon --refresh-dependencies run -Pargs=8080,production
