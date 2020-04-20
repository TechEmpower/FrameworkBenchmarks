FROM golang:1.14

WORKDIR /home
COPY . .

RUN mkdir -p bin
ENV GOPATH /home
ENV path ${GOPATH}/bin:${PATH}

RUN go get github.com/go-martini/martini
RUN go get github.com/lib/pq

CMD go run randomNumber.go sanitizeQueries.go main.go
