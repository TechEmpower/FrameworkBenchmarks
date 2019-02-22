FROM golang:1.11.5

ENV GO111MODULE on
WORKDIR /go-std

COPY ./src /go-std

RUN go get github.com/valyala/quicktemplate/qtc
RUN go mod download

RUN go generate ./templates
RUN go build -ldflags="-s -w" -o app .

CMD ./app -db mongo -prefork -db_connection_string "tfb-database"
