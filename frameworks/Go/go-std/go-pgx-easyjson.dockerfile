FROM docker.io/golang:1.19

WORKDIR /go-std

COPY ./src /go-std

RUN go mod download

# generate easyjson and quicktemplate code
RUN go generate -x ./...

RUN GOAMD64=v3 go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -db pgx -easyjson
