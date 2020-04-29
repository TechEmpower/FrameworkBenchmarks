FROM golang:1.14

ADD ./src /pine
WORKDIR /pine
RUN go mod download
RUN go build -o server .
CMD ./server