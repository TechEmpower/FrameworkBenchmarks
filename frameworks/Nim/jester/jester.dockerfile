FROM nimlang/nim:0.19.0

ADD ./ /jester
WORKDIR /jester
RUN nimble install -y httpbeast@#v0.2.1
RUN nimble c -d:release --threads:on -y techempower.nim

CMD ./techempower
