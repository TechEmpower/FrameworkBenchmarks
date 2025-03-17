FROM docker.io/golang:latest

WORKDIR /gnet

COPY ./src /gnet

RUN GOAMD64=v3 go build -o app -tags=poll_opt -gcflags="-l=4" -ldflags="-s -w" .

EXPOSE 8080

CMD ./app
