FROM docker.io/golang:1.19

ENV GO111MODULE on
WORKDIR /go-std

COPY ./src /go-std

RUN go mod download

# generate easyjson and quicktemplate code
RUN go generate -x ./...

RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -db mysql -db_connection_string "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world?interpolateParams=true"
