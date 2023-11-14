FROM docker.io/golang:1.19

ADD ./ /clevergo
WORKDIR /clevergo

RUN GOAMD64=v3 go build -o app main.go

EXPOSE 8080

CMD ./app
