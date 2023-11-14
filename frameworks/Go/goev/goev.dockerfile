FROM docker.io/golang:1.20

WORKDIR /goev

COPY ./src /goev

RUN go mod download

RUN GOAMD64=v3 go build -o ./app main.go

EXPOSE 8080

CMD ./app
