FROM nimlang/nim:latest

ADD ./ /prologue
WORKDIR /prologue
RUN nimble install -y
RUN nim c -r -d:release --threads:on
