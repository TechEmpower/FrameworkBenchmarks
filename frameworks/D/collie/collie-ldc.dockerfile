FROM techempower/d-lang:0.1

COPY ./ ./

RUN dub upgrade --verbose
RUN dub build -f -b release --compiler=ldc2

CMD ["./http"]
