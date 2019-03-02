FROM golang:1.11.5

ADD ./src/postgres/ /go-std
ADD ./src/templates/ /go-std/src/templates
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/lib/pq
RUN go get github.com/valyala/quicktemplate
RUN go get -u github.com/valyala/quicktemplate/qtc

RUN qtc -file src/templates/fortunes.qtpl
RUN go build -o hello_postgres .

CMD ./hello_postgres
