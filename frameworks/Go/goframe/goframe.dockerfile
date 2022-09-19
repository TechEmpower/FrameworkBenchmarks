FROM docker.io/golang:1.19

ADD     ./src /goframe
WORKDIR /goframe

RUN go generate -x ./...

RUN GOAMD64=v3 go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
