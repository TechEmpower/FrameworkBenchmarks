FROM dlanguage/ldc:1.7.0

ADD ./ /hunt
WORKDIR /hunt

RUN git clone https://github.com/nodejs/http-parser.git && \
    cd http-parser && \
    make package
    
RUN dub upgrade --verbose
RUN dub build -f --arch=x86_64 --build=release

CMD ["./hunt-benchmark"]
