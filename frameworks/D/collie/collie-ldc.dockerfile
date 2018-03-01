FROM tfb/d-lang:latest

COPY ./ ./

RUN dub upgrade --verbose
RUN dub build -f -b release --compiler=ldc2

CMD ["./http"]
