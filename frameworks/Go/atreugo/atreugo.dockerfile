FROM golang:1.17

WORKDIR /atreugo

COPY ./src /atreugo

RUN go get -u github.com/valyala/quicktemplate/qtc

RUN go generate ./templates
RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
