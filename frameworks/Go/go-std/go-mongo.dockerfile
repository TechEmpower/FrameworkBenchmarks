FROM golang:1.11.5

ADD ./ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN apt update -yqq && apt install -yqq libsasl2-dev
RUN go get gopkg.in/mgo.v2
RUN go build -o hello_mongo hello_mongo.go
CMD ./hello_mongo
