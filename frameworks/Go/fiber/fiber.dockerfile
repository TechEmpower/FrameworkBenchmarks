FROM golang:1.13

ADD ./ /fiber
WORKDIR /fiber

RUN mkdir bin
ENV GOPATH /fiber
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/fenny/fiber

RUN go build -o server server.go
CMD ./server
