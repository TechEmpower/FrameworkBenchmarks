FROM golang:1.14

WORKDIR /fiber

COPY ./src /fiber

RUN go get github.com/valyala/quicktemplate/qtc

RUN go mod download

RUN go generate ./templates

RUN go build -ldflags="-s -w" -o server .

CMD ./server -prefork
