FROM golang:1.13

ENV GO111MODULE on

WORKDIR /gnet

COPY ./src /gnet

RUN go mod download

RUN go build -o app -gcflags="-l=4" -ldflags="-s -w" .

CMD ./app
