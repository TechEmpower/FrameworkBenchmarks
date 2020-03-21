package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"
	"runtime"
	"time"

	"github.com/panjf2000/gnet"
)

type httpServer struct {
	*gnet.EventServer
}

type httpCodec struct {
}

func (hc *httpCodec) Encode(c gnet.Conn, buf []byte) (out []byte, err error) {
	return buf, nil
}

func (hc *httpCodec) Decode(c gnet.Conn) (out []byte, err error) {
	buf := c.Read()
	c.ResetBuffer()

	// process the pipeline
	var (
		leftover []byte
		ok       bool
	)
pipeline:
	if leftover, ok = parseReq(buf); ok {
		out = appendResp(out)
		buf = leftover
		goto pipeline
	}
	// request not ready, yet
	return
}

func (hs *httpServer) OnInitComplete(srv gnet.Server) (action gnet.Action) {
	log.Printf("HTTP server is listening on %s (multi-cores: %t, loops: %d)\n",
		srv.Addr.String(), srv.Multicore, srv.NumEventLoop)
	return
}

func (hs *httpServer) React(frame []byte, c gnet.Conn) (out []byte, action gnet.Action) {
	// handle the request
	out = frame
	return
}

func init() {
	runtime.GOMAXPROCS(runtime.NumCPU() * 2)
}

func main() {
	var port int
	var multicore bool

	// Example command: go run main.go --port 8080 --multicore=true
	flag.IntVar(&port, "port", 8080, "server port")
	flag.BoolVar(&multicore, "multicore", true, "multicore")
	flag.Parse()

	http := new(httpServer)
	hc := new(httpCodec)

	// Start serving!
	log.Fatal(gnet.Serve(http, fmt.Sprintf("tcp://:%d", port), gnet.WithMulticore(multicore), gnet.WithCodec(hc)))
}

// appendResp will append a valid http response to the provide bytes.
// The status param should be the code plus text such as "200 OK".
// The head parameter should be a series of lines ending with "\r\n" or empty.
func appendResp(b []byte) []byte {
	b = append(b, "HTTP/1.1 200 OK\r\nServer: gnet\r\nContent-Type: text/plain\r\nDate: "...)
	b = time.Now().AppendFormat(b, "Mon, 02 Jan 2006 15:04:05 GMT")
	b = append(b, "\r\nContent-Length: 13\r\n\r\nHello, World!"...)
	return b
}

var brn = []byte("\r\n\r\n")

// parseReq is a very simple http request parser. This operation
// waits for the entire payload to be buffered before returning a
// valid request.
func parseReq(data []byte) ([]byte, bool) {
	if i := bytes.Index(data, brn); i != -1 {
		return data[i+4:], true
	}

	// not enough data
	return data, false
}
