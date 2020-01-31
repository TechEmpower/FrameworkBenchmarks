FROM golang:1.13

WORKDIR /fiber

COPY ./src /fiber

RUN go mod download
RUN go build -o server *.go

CMD ./server
