FROM nimlang/nim:1.0.10

ADD ./ /prologue

WORKDIR /prologue

RUN nimble install -y

RUN nimble c --d:release --threads:on -y app.nim

CMD ./app
