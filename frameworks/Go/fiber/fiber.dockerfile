FROM docker.io/golang:1.20

WORKDIR /fiber

COPY ./src /fiber

RUN go mod download

RUN go generate -x ./templates

RUN GOAMD64=v3 go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
