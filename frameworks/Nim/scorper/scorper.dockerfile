FROM nimlang/nim:alpine

ENV PATH $PATH:/root/.nimble/bin

ADD ./ /scorper
WORKDIR /scorper

RUN nimble install -d -y
RUN nimble c -d:ChronosAsync -d:GzipEnable=false -d:release --opt:speed -d:danger -o:scorper_bench_bin ./scorper_bench.nim

EXPOSE 8080

CMD ./scorper_bench_bin
