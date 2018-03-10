FROM tfb/leiningen-java8:latest
ADD ./ /macchiato
WORKDIR /macchiato
ENV NODE_ENV=production
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt install -y nodejs
RUN lein package
CMD node target/release/hello.js
