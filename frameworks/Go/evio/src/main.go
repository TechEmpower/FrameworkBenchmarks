// Copyright 2017 Joshua J Baker. All rights reserved.
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file.

package main

import (
	"flag"
	"fmt"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/tidwall/evio"
)

var res string

type request struct {
	proto, method string
	path, query   string
	head, body    string
	remoteAddr    string
}

func main() {
	var port int
	var loops int
	var unixsocket string
	var stdlib bool

	flag.StringVar(&unixsocket, "unixsocket", "", "unix socket")
	flag.IntVar(&port, "port", 8080, "server port")
	flag.BoolVar(&stdlib, "stdlib", false, "use stdlib")
	flag.IntVar(&loops, "loops", -1, "num loops")
	flag.Parse()

	res = "Hello, World!"

	var events evio.Events
	events.NumLoops = loops

	events.Serving = func(srv evio.Server) (action evio.Action) {
		log.Printf("http server started on port %d (loops: %d)", port, srv.NumLoops)
		if unixsocket != "" {
			log.Printf("http server started at %s", unixsocket)
		}
		if stdlib {
			log.Printf("http server is using stdlib")
		}
		return
	}

	events.Opened = func(c evio.Conn) (out []byte, opts evio.Options, action evio.Action) {
		c.SetContext(&evio.InputStream{})
		return
	}

	events.Closed = func(c evio.Conn, err error) (action evio.Action) {
		return
	}

	events.Data = func(c evio.Conn, in []byte) (out []byte, action evio.Action) {
		if in == nil {
			return
		}
		is := c.Context().(*evio.InputStream)
		data := is.Begin(in)

		// buf := bufio.NewReader(bytes.NewReader(data))
		// req, err := http.ReadRequest(buf)
		// if err != nil {
		// 	log.Println(err)
		// 	is.End(data)
		// 	return
		// }
		// t := &http.Response{
		// 	Status:        "200 OK",
		// 	StatusCode:    200,
		// 	Proto:         "HTTP/1.1",
		// 	ProtoMajor:    1,
		// 	ProtoMinor:    1,
		// 	Body:          ioutil.NopCloser(bytes.NewBufferString(res)),
		// 	ContentLength: int64(len(res)),
		// 	Request:       req,
		// 	Header:        make(http.Header, 0),
		// }
		// resp := bytes.NewBuffer(nil)
		// t.Write(resp)
		// out = resp.Bytes()

		// process the pipeline
		var req request
		for {
			leftover, err := parsereq(data, &req)
			if err != nil {
				// bad thing happened
				out = appendresp(out, "500 Error", "", err.Error()+"\n")
				action = evio.Close
				break
			} else if len(leftover) == len(data) {
				// request not ready, yet
				break
			}
			// handle the request
			req.remoteAddr = c.RemoteAddr().String()
			out = appendhandle(out, &req)
			data = leftover
		}
		is.End(data)
		return
	}
	var ssuf string
	if stdlib {
		ssuf = "-net"
	}
	// We at least want the single http address.
	addrs := []string{fmt.Sprintf("tcp"+ssuf+"://:%d", port)}
	if unixsocket != "" {
		addrs = append(addrs, fmt.Sprintf("unix"+ssuf+"://%s", unixsocket))
	}
	// Start serving!
	log.Fatal(evio.Serve(events, addrs...))
}

// appendhandle handles the incoming request and appends the response to
// the provided bytes, which is then returned to the caller.
func appendhandle(b []byte, req *request) []byte {
	return appendresp(b, "200 OK", "", res)
}

// appendresp will append a valid http response to the provide bytes.
// The status param should be the code plus text such as "200 OK".
// The head parameter should be a series of lines ending with "\r\n" or empty.
func appendresp(b []byte, status, head, body string) []byte {
	b = append(b, "HTTP/1.1"...)
	b = append(b, ' ')
	b = append(b, status...)
	b = append(b, '\r', '\n')
	b = append(b, "Server: evio\r\n"...)
	b = append(b, "Content-Type: text/plain\r\n"...)
	b = append(b, "Date: "...)
	b = time.Now().AppendFormat(b, "Mon, 02 Jan 2006 15:04:05 GMT")
	b = append(b, '\r', '\n')
	if len(body) > 0 {
		b = append(b, "Content-Length: "...)
		b = strconv.AppendInt(b, int64(len(body)), 10)
		b = append(b, '\r', '\n')
	}
	b = append(b, head...)
	b = append(b, '\r', '\n')
	if len(body) > 0 {
		b = append(b, body...)
	}
	return b
}

// parsereq is a very simple http request parser. This operation
// waits for the entire payload to be buffered before returning a
// valid request.
func parsereq(data []byte, req *request) (leftover []byte, err error) {
	sdata := string(data)
	var i, s int
	var top string
	var clen int
	var q = -1
	// method, path, proto line
	for ; i < len(sdata); i++ {
		if sdata[i] == ' ' {
			req.method = sdata[s:i]
			for i, s = i+1, i+1; i < len(sdata); i++ {
				if sdata[i] == '?' && q == -1 {
					q = i - s
				} else if sdata[i] == ' ' {
					if q != -1 {
						req.path = sdata[s:q]
						req.query = req.path[q+1 : i]
					} else {
						req.path = sdata[s:i]
					}
					for i, s = i+1, i+1; i < len(sdata); i++ {
						if sdata[i] == '\n' && sdata[i-1] == '\r' {
							req.proto = sdata[s:i]
							i, s = i+1, i+1
							break
						}
					}
					break
				}
			}
			break
		}
	}
	if req.proto == "" {
		return data, fmt.Errorf("malformed request")
	}
	top = sdata[:s]
	for ; i < len(sdata); i++ {
		if i > 1 && sdata[i] == '\n' && sdata[i-1] == '\r' {
			line := sdata[s : i-1]
			s = i + 1
			if line == "" {
				req.head = sdata[len(top)+2 : i+1]
				i++
				if clen > 0 {
					if len(sdata[i:]) < clen {
						break
					}
					req.body = sdata[i : i+clen]
					i += clen
				}
				return data[i:], nil
			}
			if strings.HasPrefix(line, "Content-Length:") {
				n, err := strconv.ParseInt(strings.TrimSpace(line[len("Content-Length:"):]), 10, 64)
				if err == nil {
					clen = int(n)
				}
			}
		}
	}
	// not enough data
	return data, nil
}
