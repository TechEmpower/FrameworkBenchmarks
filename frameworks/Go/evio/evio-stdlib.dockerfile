FROM golang:1.12

ENV GO111MODULE on
WORKDIR /evio

COPY ./src /evio

RUN go mod download

RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -stdlib
