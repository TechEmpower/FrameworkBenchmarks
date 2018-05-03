FROM golang:1.10.1

ADD ./ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN apt update -yqq
RUN apt install -yqq libsasl2-dev > /dev/null
RUN go get gopkg.in/mgo.v2
CMD go run hello_mongo.go
