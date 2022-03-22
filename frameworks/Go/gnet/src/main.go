package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"log"
	"runtime"
	"sync/atomic"
	"time"

	"github.com/panjf2000/gnet/v2"
)

type httpServer struct {
	gnet.BuiltinEventEngine

	addr      string
	multicore bool
	eng       gnet.Engine
}

type httpCodec struct {
	delimiter []byte
	buf       []byte
}

func (hc *httpCodec) appendResponse() {
	hc.buf = append(hc.buf, "HTTP/1.1 200 OK\r\nServer: gnet\r\nContent-Type: text/plain\r\nDate: "...)
	//hc.buf = time.Now().AppendFormat(hc.buf, "Mon, 02 Jan 2006 15:04:05 GMT")
	hc.buf = append(hc.buf, NowTimeFormat()...)
	hc.buf = append(hc.buf, "\r\nContent-Length: 13\r\n\r\nHello, World!"...)
}

var errCRLFNotFound = errors.New("CRLF not found")

func (hc *httpCodec) parse(data []byte) (int, error) {
	if idx := bytes.Index(data, hc.delimiter); idx != -1 {
		return idx + 4, nil
	}
	return -1, errCRLFNotFound
}

func (hc *httpCodec) reset() {
	hc.buf = hc.buf[:0]
}

func (hs *httpServer) OnBoot(eng gnet.Engine) gnet.Action {
	hs.eng = eng
	log.Printf("echo server with multi-core=%t is listening on %s\n", hs.multicore, hs.addr)
	return gnet.None
}

func (hs *httpServer) OnOpen(c gnet.Conn) ([]byte, gnet.Action) {
	c.SetContext(&httpCodec{delimiter: []byte("\r\n\r\n")})
	return nil, gnet.None
}

func (hs *httpServer) OnTraffic(c gnet.Conn) gnet.Action {
	buf, _ := c.Next(-1)
	hc := c.Context().(*httpCodec)
pipeline:
	nextOffset, err := hc.parse(buf)
	if err != nil {
		goto response
	}
	hc.appendResponse()
	buf = buf[nextOffset:]
	if len(buf) > 0 {
		goto pipeline
	}
response:
	c.Write(hc.buf)
	hc.reset()
	return gnet.None
}

var now atomic.Value

func ticktock() {
	now.Store(nowTimeFormat())
	for range time.Tick(time.Second) {
		now.Store(nowTimeFormat())
	}
}

func NowTimeFormat() string {
	return now.Load().(string)
}

func nowTimeFormat() string {
	return time.Now().Format("Mon, 02 Jan 2006 15:04:05 GMT")
}

func init() {
	runtime.GOMAXPROCS(runtime.NumCPU() * 2)
	now.Store(nowTimeFormat())
	go ticktock()
}

func main() {
	var port int
	var multicore bool

	// Example command: go run main.go --port 8080 --multicore=true
	flag.IntVar(&port, "port", 8080, "server port")
	flag.BoolVar(&multicore, "multicore", true, "multicore")
	flag.Parse()

	hs := &httpServer{addr: fmt.Sprintf("tcp://:%d", port), multicore: multicore}

	// Start serving!
	log.Println("server exits:", gnet.Run(hs, hs.addr, gnet.WithMulticore(multicore)))
}
