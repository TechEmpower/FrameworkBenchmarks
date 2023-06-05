FROM golang:1.18

ENV GO111MODULE=on

ADD ./ /hertz
COPY ./templates /templates
WORKDIR /hertz

RUN go mod tidy

RUN go build -o hello hello.go

EXPOSE 8080

CMD ./hello
