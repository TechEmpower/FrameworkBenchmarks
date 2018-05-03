FROM clojure:lein-2.8.1
WORKDIR /macchiato
COPY env env
COPY src src
COPY project.clj project.clj
ENV NODE_ENV=production
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt install -yqq nodejs
RUN lein package
CMD ["node", "target/release/hello.js"]
