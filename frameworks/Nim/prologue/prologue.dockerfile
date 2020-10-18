FROM nimlang/nim:latest

ADD ./ /prologue
WORKDIR /prologue
RUN nimble run --d:release --threads:on
