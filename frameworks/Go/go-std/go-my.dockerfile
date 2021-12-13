FROM golang:1.14

ENV GO111MODULE on
WORKDIR /go-std

COPY ./src /go-std

RUN go get github.com/valyala/quicktemplate/qtc
RUN go get -u github.com/mailru/easyjson/...
RUN go mod download

RUN go generate ./templates
RUN easyjson -pkg
RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -db mysql -db_connection_string "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world?interpolateParams=true"
