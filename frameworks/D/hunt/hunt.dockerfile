FROM dlangchina/dlang-dmd:latest

RUN apt update -yqq && apt install -yqq git make libpq-dev

ADD ./ /hunt
WORKDIR /hunt

RUN git clone https://github.com/nodejs/http-parser.git && \
    cd http-parser && \
    make package
    
RUN dub upgrade --verbose
RUN dub build -f --arch=x86_64 --build=release -c=lite

CMD ["./hunt-minihttp"]
