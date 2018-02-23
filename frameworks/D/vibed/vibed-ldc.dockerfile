FROM tfb/d-lang:latest

COPY ./ ./

RUN dub upgrade --verbose
RUN dub build -b release --compiler=ldc2 --combined --verbose

CMD ["./fwb"]
