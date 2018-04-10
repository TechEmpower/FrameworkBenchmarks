FROM golang:1.10.1

ADD ./ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/lib/pq
CMD go run hello_postgres.go
