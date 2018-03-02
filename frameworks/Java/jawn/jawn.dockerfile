FROM tfb/java8:latest
ADD ./ /jawn
WORKDIR /jawn
RUN ./gradlew clean
CMD ./gradlew --no-daemon --refresh-dependencies run -Pargs=8080,production
