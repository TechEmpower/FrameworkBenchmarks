FROM docker.io/golang:1.20

WORKDIR /hertz

ENV GO111MODULE=on

ADD ./ /hertz
COPY ./templates /templates

RUN go mod download
RUN GOAMD64=v3 go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
