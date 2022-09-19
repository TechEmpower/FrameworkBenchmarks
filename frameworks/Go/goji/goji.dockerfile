FROM docker.io/golang:1.19

ADD ./src /goji
WORKDIR /goji

RUN go build -o server ./goji/server.go

EXPOSE 8080

CMD ./server
