FROM golang:1.14

WORKDIR /gramework

COPY ./src /gramework

RUN go mod download

RUN go build -ldflags="-s -w" -o app .

CMD ./app
