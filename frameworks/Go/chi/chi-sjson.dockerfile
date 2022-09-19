FROM docker.io/golang:1.19

ADD ./src/chi-sjson /chi
WORKDIR /chi

RUN go build -o server .

EXPOSE 8080

CMD ./server