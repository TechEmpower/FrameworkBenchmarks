FROM nimlang/nim:latest

ADD ./ /prologue

WORKDIR /prologue

RUN nimble install -y

RUN nimble c --d:release --threads:on -y app.nim

CMD ./app
