FROM nimlang/nim:1.0.4

RUN apt-get update -y
RUN apt-get install -y libpq-dev

ADD ./ /jester
WORKDIR /jester

RUN nimble c -d:release --threads:on -y techempower.nim

CMD ./techempower
