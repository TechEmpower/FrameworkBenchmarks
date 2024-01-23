FROM dlang2/ldc-ubuntu:1.15.0

ADD ./ /collie
WORKDIR /collie

RUN dub upgrade --verbose
RUN dub build -f -b release --compiler=ldc2

EXPOSE 8085

CMD ["./http"]
