FROM docker.io/golang:1.19

WORKDIR /gin
COPY ./gin-std /gin

RUN go build -o hello

EXPOSE 8080

CMD ./hello
