FROM techempower/leiningen-java8:0.1

ADD ./ /http-kit
WORKDIR /http-kit
RUN lein clean
RUN lein deps
RUN lein uberjar
