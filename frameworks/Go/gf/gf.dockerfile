FROM golang:1.12

WORKDIR /gf

COPY ./src /gf

RUN go mod download

RUN go build -ldflags="-s -w" -o app .

CMD ./app
