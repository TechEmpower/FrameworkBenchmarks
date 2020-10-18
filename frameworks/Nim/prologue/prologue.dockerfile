FROM nimlang/nim:1.14.0

ADD ./ /prologue
WORKDIR /prologue
RUN nim c -r --d:release --threads:on app.nim

CMD ./app
