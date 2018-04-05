FROM techempower/d-lang:0.1

COPY ./ ./

RUN dub upgrade --verbose
RUN dub build -b release --combined

CMD ["./fwb"]
