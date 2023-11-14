FROM nimlang/nim:1.6.12

ADD ./ /prologue

WORKDIR /prologue

RUN nimble c --d:release --threads:on -y app.nim

EXPOSE 8080

CMD ./app

