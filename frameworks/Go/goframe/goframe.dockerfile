FROM golang:1.17

ADD     ./src /goframe
WORKDIR /goframe

RUN go generate -x ./...

RUN     go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
