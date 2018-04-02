FROM techempower/d-lang:0.1

COPY ./ ./

RUN apt install -yqq zlib1g-dev

RUN dub upgrade --verbose
RUN dub build -b release --compiler=ldc2 --combined --verbose

CMD ["./fwb"]
