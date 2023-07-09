FROM docker.io/golang:1.20

WORKDIR /goev

COPY ./src /goev

RUN go build -o app .

EXPOSE 8080

CMD ./app
