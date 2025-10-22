package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"log"
	"runtime"
	"strconv"
	"sync/atomic"
	"time"

	"github.com/evanphx/wildcat"
	"github.com/panjf2000/gnet/v2"
)

type httpServer struct {
	gnet.BuiltinEventEngine

	addr      string
	multicore bool
	eng       gnet.Engine
}

type httpCodec struct {
	parser        *wildcat.HTTPParser
	contentLength int
	buf           []byte
}

var CRLF = []byte("\r\n\r\n")

func (hc *httpCodec) parse(data []byte) (int, error) {
	// Perform a legit HTTP request parsing.
	bodyOffset, err := hc.parser.Parse(data)
	if err != nil {
		return 0, err
	}

	// First check if the Content-Length header is present.
	contentLength := hc.getContentLength()
	if contentLength > -1 {
		return bodyOffset + contentLength, nil
	}

	// If the Content-Length header is not found,
	// we need to find the end of the body section.
	if idx := bytes.Index(data, CRLF); idx != -1 {
		return idx + 4, nil
	}

	return 0, errors.New("invalid http request")
}

var contentLengthKey = []byte("Content-Length")

func (hc *httpCodec) getContentLength() int {
	if hc.contentLength != -1 {
		return hc.contentLength
	}

	val := hc.parser.FindHeader(contentLengthKey)
	if val != nil {
		i, err := strconv.ParseInt(string(val), 10, 0)
		if err == nil {
			hc.contentLength = int(i)
		}
	}

	return hc.contentLength
}

func (hc *httpCodec) resetParser() {
	hc.contentLength = -1
}

func (hc *httpCodec) reset() {
	hc.resetParser()
	hc.buf = hc.buf[:0]
}

func (hc *httpCodec) appendResponse() {
	hc.buf = append(hc.buf, "HTTP/1.1 200 OK\r\nServer: gnet\r\nContent-Type: text/plain\r\nDate: "...)
	hc.buf = append(hc.buf, NowTimeFormat()...)
	hc.buf = append(hc.buf, "\r\nContent-Length: 13\r\n\r\nHello, World!"...)
}

func (hs *httpServer) OnBoot(eng gnet.Engine) gnet.Action {
	hs.eng = eng
	log.Printf("echo server with multi-core=%t is listening on %s\n", hs.multicore, hs.addr)
	return gnet.None
}

func (hs *httpServer) OnOpen(c gnet.Conn) ([]byte, gnet.Action) {
	c.SetContext(&httpCodec{parser: wildcat.NewHTTPParser()})
	return nil, gnet.None
}

func (hs *httpServer) OnTraffic(c gnet.Conn) gnet.Action {
	hc := c.Context().(*httpCodec)
	buf, _ := c.Next(-1)

pipeline:
	nextOffset, err := hc.parse(buf)
	if err != nil {
		goto response
	}
	hc.resetParser()
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
