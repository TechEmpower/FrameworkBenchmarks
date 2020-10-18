FROM nimlang/nim:1.14.0

ADD ./ /prologue
WORKDIR /prologue
RUN nimble run --d:release --threads:on
