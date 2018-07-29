FROM golang:1.10.1

ADD ./ /webgo
WORKDIR /webgo

RUN mkdir bin
ENV GOPATH /webgo
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/hoisie/web

CMD go run src/hello/hello.go
