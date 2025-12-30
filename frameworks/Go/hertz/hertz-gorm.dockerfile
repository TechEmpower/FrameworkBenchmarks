FROM golang:1.24.2

ENV GO111MODULE=on
WORKDIR /src/
ADD ./hertz-gorm /src/

RUN go mod download
RUN GOAMD64=v3 go build -o app

EXPOSE 8080

ENTRYPOINT ["/src/app"]
