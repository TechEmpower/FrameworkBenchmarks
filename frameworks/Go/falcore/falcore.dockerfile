FROM tfb/go-lang:latest
ADD ./ /falcore
WORKDIR /falcore

RUN mkdir bin
ENV GOPATH /falcore
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get ./...
CMD go run src/framework_benchmarks/falcore.go
