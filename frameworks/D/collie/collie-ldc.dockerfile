FROM dlanguage/ldc:1.7.0

ADD ./ /collie
WORKDIR /collie

RUN dub upgrade --verbose
RUN dub build -f -b release --compiler=ldc2

CMD ["./http"]
