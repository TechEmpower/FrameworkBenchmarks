FROM dlangchina/dlang-dmd:latest

ADD ./ /hunt
WORKDIR /hunt

RUN apt update -y && apt install -y --no-install-recommends git && rm -rf /var/lib/apt/lists/* && rm -rf /var/cache/apt/*

RUN git clone https://github.com/nodejs/http-parser.git && \
    cd http-parser && \
    make package
    
RUN dub upgrade --verbose
RUN dub build --build=release --arch=x86_64 --config=postgresql --compiler=dmd -f

CMD ["./hunt-minihttp"]
