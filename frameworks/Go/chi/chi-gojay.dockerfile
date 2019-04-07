FROM golang:1.12

ADD ./ /chi
WORKDIR /chi

RUN mkdir bin
ENV GOPATH /chi
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/francoispqt/gojay
RUN go get github.com/jackc/pgx
RUN go get github.com/go-chi/chi

RUN go build -o server src/chi-gojay/*.go
CMD ./server