FROM golang:1.12

ENV GO111MODULE on
WORKDIR /atreugo

COPY ./src /atreugo

RUN go mod download

RUN go build -ldflags="-s -w" -o app .

CMD ./app -prefork
