FROM nimlang/nim:latest

ADD ./ /nim-stdlib

WORKDIR /nim-stdlib

RUN nimble c --d:release --threads:on -y nim-stdlib.nim

EXPOSE 8080

CMD ./bin/nim-stdlib
