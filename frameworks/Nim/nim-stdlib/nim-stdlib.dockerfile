FROM nimlang/nim:latest

ADD ./ /nimstdlib

WORKDIR /nimstdlib

RUN nimble c --d:release --threads:on -y src/nimstdlib.nim

EXPOSE 8080

CMD ./bin/nimstdlib
