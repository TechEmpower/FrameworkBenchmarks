FROM golang:1.13

WORKDIR /fasthttp

COPY ./src /fasthttp

RUN go get github.com/valyala/quicktemplate/qtc
RUN go get -u github.com/mailru/easyjson/...
RUN go mod download

RUN go generate ./templates
# RUN easyjson -pkg
# RUN easyjson -all src/common/common.go
RUN go build -o app-pg -gcflags='-l=4' -ldflags="-s -w" ./server-postgresql

CMD ./app-pg -prefork
