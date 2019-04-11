FROM dlang2/ldc-ubuntu:1.15.0

ADD ./ /collie
WORKDIR /collie

RUN dub upgrade --verbose
RUN dub build -f -b release-nobounds --compiler=ldc2

CMD ["./http"]
