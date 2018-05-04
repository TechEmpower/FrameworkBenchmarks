FROM dlanguage/ldc:1.7.0

ADD ./ /kiss
WORKDIR /kiss

RUN dub upgrade --verbose
RUN dub build -f --arch=x86_64 --build=release

CMD ["./kiss-benchmark"]
