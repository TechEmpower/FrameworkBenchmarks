FROM golang:1.17

WORKDIR /silverlining

COPY ./src /silverlining

RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
