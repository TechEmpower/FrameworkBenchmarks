FROM golang:1.14

WORKDIR /gearbox

COPY ./src /gearbox

RUN go generate -x ./templates

RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
