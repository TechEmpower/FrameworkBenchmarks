FROM dlanguage/ldc:1.7.0

ADD ./ /hunt
WORKDIR /hunt

RUN dub upgrade --verbose

RUN dub build -f -b release -v

CMD ["./website"]
