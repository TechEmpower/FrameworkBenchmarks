FROM tfb/leiningen-java8:latest
ADD ./ /luminus
WORKDIR /luminus
RUN lein clean
RUN lein uberjar
CMD java -server -jar target/hello.jar
