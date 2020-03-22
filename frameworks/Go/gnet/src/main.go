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
	brn []byte
}

func (hc *httpCodec) Encode(c gnet.Conn, buf []byte) (out []byte, err error) {
	return buf, nil
}

func (hc *httpCodec) Decode(c gnet.Conn) (out []byte, err error) {
	buf := c.Read()
	c.ResetBuffer()

	// process the pipeline
	var i int
pipeline:
	if i = bytes.Index(buf, hc.brn); i != -1 {
		out = appendResp(out)
		buf = buf[i+4:]
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
	hc := &httpCodec{brn: []byte("\r\n\r\n")}

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
