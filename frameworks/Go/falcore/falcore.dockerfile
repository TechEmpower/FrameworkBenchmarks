FROM golang:1.14

ADD ./ /falcore
WORKDIR /falcore

RUN mkdir bin
ENV GOPATH /falcore
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get ./...

EXPOSE 8080

CMD go run src/framework_benchmarks/falcore.go
