FROM dlanguage/ldc:1.7.0

ADD ./ /vibed
WORKDIR /vibed

RUN dub upgrade --verbose
RUN dub build -b release --combined

CMD ["./fwb"]
