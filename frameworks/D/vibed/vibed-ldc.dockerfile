FROM dlanguage/ldc:1.7.0

ADD ./ /vibed
WORKDIR /vibed

RUN apt update -yqq && apt install -yqq zlib1g-dev

RUN dub upgrade --verbose
RUN dub build -b release --compiler=ldc2 --combined --verbose

CMD ["./fwb"]
