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
	delimiter []byte
}

func (hc *httpCodec) Encode(c gnet.Conn, buf []byte) (out []byte, err error) {
	return buf, nil
}

func (hc *httpCodec) Decode(c gnet.Conn) (out []byte, err error) {
	buf := c.Read()
	if buf == nil {
		return
	}
	c.ResetBuffer()

	// process the pipeline
	var i int
pipeline:
	if i = bytes.Index(buf, hc.delimiter); i != -1 {
		out = append(out, "HTTP/1.1 200 OK\r\nServer: gnet\r\nContent-Type: text/plain\r\nDate: "...)
		out = time.Now().AppendFormat(out, "Mon, 02 Jan 2006 15:04:05 GMT")
		out = append(out, "\r\nContent-Length: 13\r\n\r\nHello, World!"...)
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
	hc := &httpCodec{delimiter: []byte("\r\n\r\n")}

	// Start serving!
	log.Fatal(gnet.Serve(http, fmt.Sprintf("tcp://:%d", port), gnet.WithMulticore(multicore), gnet.WithCodec(hc)))
}
