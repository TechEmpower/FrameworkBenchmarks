FROM golang:1.10.1

COPY ./ /iris
WORKDIR /iris

RUN mkdir bin
ENV GOPATH /iris
ENV PATH ${GOPATH}/bin:${PATH}

RUN rm -rf ./pkg/*
RUN go get github.com/kataras/iris
RUN go get github.com/lib/pq

CMD go run main.go
