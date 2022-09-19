FROM docker.io/golang:1.19

ADD ./ /webgo
WORKDIR /webgo

RUN mkdir bin
ENV GOPATH /webgo
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/hoisie/web

EXPOSE 8080

CMD go run src/hello/hello.go
