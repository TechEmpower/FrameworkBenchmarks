FROM docker.io/golang:1.24.2

WORKDIR /gin
COPY ./gin-std /gin

RUN go mod download

RUN GOAMD64=v3 go build -o hello

EXPOSE 8080

CMD ./hello
