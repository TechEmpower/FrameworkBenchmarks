FROM docker.io/golang:1.21

WORKDIR /sprapp

ENV GO111MODULE=on

ADD ./ /sprapp

RUN go mod download
RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
