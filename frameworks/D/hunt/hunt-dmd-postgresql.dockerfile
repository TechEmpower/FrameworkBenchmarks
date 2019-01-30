FROM dlanguage/ldc:1.7.0

ADD ./ /hunt
WORKDIR /hunt

RUN git clone https://github.com/nodejs/http-parser.git && \
    cd http-parser && \
    make package
    
RUN apt update -yqq && apt install -yqq libpq-dev

RUN dub upgrade --verbose
RUN dub build --build=release --arch=x86_64 --config=postgresql

CMD ["./hunt-minihttp"]
