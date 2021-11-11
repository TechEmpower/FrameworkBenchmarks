FROM golang:1.14

ADD ./ /chi
WORKDIR /chi

RUN mkdir bin
ENV GOPATH /chi
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/mailru/easyjson/...
RUN go get github.com/go-sql-driver/mysql
RUN go get github.com/go-chi/chi

RUN go build -o server src/chi/*.go

EXPOSE 8080

CMD ./server -prefork
