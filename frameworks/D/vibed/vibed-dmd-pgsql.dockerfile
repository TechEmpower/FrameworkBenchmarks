FROM tfb/d-lang:latest

COPY ./ ./

RUN dub upgrade --verbose
RUN dub build -b release --combined --config=postgresql

CMD ["./fwb"]
