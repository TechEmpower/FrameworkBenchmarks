FROM docker.io/golang:1.19

WORKDIR /gnet

COPY ./src /gnet

RUN go mod tidy

RUN go build -o app -tags=poll_opt -gcflags="-l=4" -ldflags="-s -w" .

EXPOSE 8080

CMD ./app
