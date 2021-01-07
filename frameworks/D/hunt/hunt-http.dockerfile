FROM dlangchina/dlang-ldc:latest

ADD ./ /hunt
WORKDIR /hunt

RUN apt-get update -y && apt-get install -y --no-install-recommends git && rm -rf /var/lib/apt/lists/* && rm -rf /var/cache/apt/*

RUN git clone https://github.com/nodejs/http-parser.git && \
    cd http-parser && \
    make package && \
    cd ..

RUN dub upgrade --verbose
RUN dub build --build=release --arch=x86_64 --compiler=ldc2 -f

EXPOSE 8080

CMD ["./hunt-minihttp"]
