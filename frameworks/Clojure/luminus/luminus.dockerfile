FROM techempower/leiningen-java8:0.1

ADD ./ /luminus
WORKDIR /luminus
RUN lein clean
RUN lein uberjar
CMD java -server -jar target/hello.jar
