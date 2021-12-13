FROM golang:1.15

ADD ./src /echo
WORKDIR /echo

RUN go build -ldflags="-s -w" -o app ./main.go

EXPOSE 8080

CMD ./app
