FROM docker.io/golang:1.19

ADD ./src/chi-sjson /chi
WORKDIR /chi

RUN GOAMD64=v3 go build -o server .

EXPOSE 8080

CMD ./server
