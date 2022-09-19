FROM docker.io/golang:1.19

WORKDIR /fiber

COPY ./src /fiber

RUN go generate -x ./templates

RUN gGOAMD64=v3 o build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
