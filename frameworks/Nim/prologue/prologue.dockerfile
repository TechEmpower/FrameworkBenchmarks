FROM nimlang/nim:1.4.0

ADD ./ /prologue

WORKDIR /prologue

RUN nimble c --d:release --threads:on -y app.nim

EXPOSE 8080

CMD ./app

