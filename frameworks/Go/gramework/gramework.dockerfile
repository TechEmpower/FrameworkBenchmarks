FROM golang:1.12

WORKDIR /gramework

COPY ./src /gramework

RUN go mod download

RUN go build -ldflags="-s -w" -o app .

CMD ./app
