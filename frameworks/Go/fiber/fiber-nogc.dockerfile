FROM golang:1.14

WORKDIR /fiber

COPY ./src /fiber

RUN go get github.com/valyala/quicktemplate/qtc

RUN go generate ./templates
RUN go build -ldflags="-s -w" -o app .

CMD ./app -prefork -nogc
