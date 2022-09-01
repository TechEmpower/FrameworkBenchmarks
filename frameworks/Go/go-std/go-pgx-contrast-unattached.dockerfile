FROM golang:1.18

ENV GO111MODULE on
WORKDIR /go-std

COPY ./src /go-std

RUN go install github.com/valyala/quicktemplate/qtc@latest
RUN go install github.com/mailru/easyjson/...@latest
RUN go mod download

RUN go generate ./templates
RUN easyjson -pkg
RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -db pgx
