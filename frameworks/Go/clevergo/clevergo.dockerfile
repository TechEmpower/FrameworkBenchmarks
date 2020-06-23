FROM golang:1.14

ADD ./ /clevergo
WORKDIR /clevergo

RUN go get

RUN go build -o app main.go
CMD ./app
