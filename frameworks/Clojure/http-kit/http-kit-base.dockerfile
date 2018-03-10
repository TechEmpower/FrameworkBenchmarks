FROM tfb/leiningen-java8:latest
ADD ./ /http-kit
WORKDIR /http-kit
RUN lein clean
RUN lein deps
RUN lein uberjar
