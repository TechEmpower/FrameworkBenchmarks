FROM techempower/go-base:0.1

RUN apt-get install -y libsasl2-dev
RUN go get gopkg.in/mgo.v2
CMD go run hello_mongo.go
