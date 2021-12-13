FROM golang:1.14

ADD ./ /goji
WORKDIR /goji

RUN mkdir bin
ENV GOPATH /goji
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/go-sql-driver/mysql
RUN go get github.com/zenazn/goji
RUN go get github.com/zenazn/goji/web

RUN go build -o server src/goji/server.go

EXPOSE 8080

CMD ./server
