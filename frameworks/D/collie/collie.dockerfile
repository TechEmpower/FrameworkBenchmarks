FROM tfb/d-lang:latest

COPY ./ ./

RUN dub upgrade --verbose
RUN dub build -f -b release

CMD ["./http"]
