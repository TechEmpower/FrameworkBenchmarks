FROM nimlang/nim:1.0.4

ADD ./ /jester
WORKDIR /jester
RUN nimble c -d:release --threads:on -y techempower.nim

CMD ./techempower
