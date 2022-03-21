FROM golang:1.18

WORKDIR /silverlining

COPY ./src /silverlining

RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
