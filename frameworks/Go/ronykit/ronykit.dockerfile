FROM docker.io/golang:1.19

WORKDIR /ronykit

COPY ./src /ronykit

RUN go mod download
RUN go mod tidy
RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
