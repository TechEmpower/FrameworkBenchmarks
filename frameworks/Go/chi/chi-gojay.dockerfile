FROM docker.io/golang:1.23.1

ADD ./src/chi-gojay /chi
WORKDIR /chi

RUN GOAMD64=v3 go build -o server .

EXPOSE 8080

CMD ./server
