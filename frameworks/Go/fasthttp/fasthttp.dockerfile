FROM docker.io/golang:1.19

WORKDIR /fasthttp

COPY ./src /fasthttp

RUN go generate -x ./templates
RUN GOAMD64=v3 go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
