FROM golang:1.11.5

ADD ./src/simple/ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN go build -o hello .
CMD ./hello
