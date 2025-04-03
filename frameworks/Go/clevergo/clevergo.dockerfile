FROM docker.io/golang:1.24.2

ADD ./ /clevergo
WORKDIR /clevergo

RUN GOAMD64=v3 go build -o app main.go

EXPOSE 8080

CMD ./app
