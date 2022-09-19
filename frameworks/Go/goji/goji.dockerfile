FROM docker.io/golang:1.19

ADD ./src /goji
WORKDIR /goji

RUN mkdir bin
ENV GOPATH /goji
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/go-sql-driver/mysql
RUN go get github.com/zenazn/goji
RUN go get github.com/zenazn/goji/web

RUN go build -o server ./goji/server.go

EXPOSE 8080

CMD ./server
