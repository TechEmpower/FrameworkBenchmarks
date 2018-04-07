FROM golang:1.10.1

ADD ./ /goji
WORKDIR /goji

RUN mkdir bin
ENV GOPATH /goji
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/go-sql-driver/mysql
RUN go get github.com/zenazn/goji
RUN go get github.com/zenazn/goji/web

CMD go run src/goji/server.go
