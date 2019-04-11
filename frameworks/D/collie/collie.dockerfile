FROM dlang2/dmd-ubuntu:2.085.1

ADD ./ /collie
WORKDIR /collie

RUN dub upgrade --verbose
RUN dub build -f -b release-nobounds

CMD ["./http"]
