FROM nimlang/nim:0.19.0

ADD ./ /httpbeast
WORKDIR /httpbeast
RUN nimble c -d:release --threads:on -y techempower.nim

CMD ./techempower
