FROM golang:1.11.5

ADD ./ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/lib/pq
RUN go build -o hello_postgres hello_postgres.go
CMD ./hello_postgres
