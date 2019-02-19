FROM golang:1.11.5

ADD ./src/mongo/ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN apt update -yqq && apt install -yqq libsasl2-dev
RUN go get gopkg.in/mgo.v2
RUN go build -o hello_mongo .
CMD ./hello_mongo -prefork
