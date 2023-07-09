package main

import (
	"fmt"
	"sync"
    "time"
    "runtime"
	"syscall"
	"sync/atomic"

	"github.com/shaovie/goev"
	"github.com/shaovie/goev/netfd"
)

var (
    httpRespHeader []byte
    httpRespContentLength []byte
	ticker *time.Ticker
    liveSecond atomic.Int64
	buffPool *sync.Pool
)

const httpHeaderS = "HTTP/1.1 200 OK\r\nConnection: close\r\nServer: goev\r\nContent-Type: text/plain\r\n"
const contentLengthS = "Content-Length: 13\r\n\r\nHello, World!"

type Http struct {
	goev.Event
}

func (h *Http) OnOpen(fd int, now int64) bool {
	if err := h.GetReactor().AddEvHandler(h, fd, goev.EV_IN); err != nil {
		return false
	}
	return true
}
func (h *Http) OnRead(fd int, now int64) bool {
	buf := buffPool.Get().([]byte) // just read
	defer buffPool.Put(buf)

	readN := 0
	for {
		if readN >= cap(buf) { // alloc new buff to read
            readN = 1 // ^_^
			break
		}
		n, err := netfd.Read(fd, buf[readN:])
		if err != nil {
			if err == syscall.EAGAIN { // epoll ET mode
				break
			}
			return false
		}
		if n > 0 { // n > 0
			readN += n
		} else { // n == 0 connection closed,  will not < 0
			if readN == 0 {
				// fmt.Println("peer closed. ", n)
			}
			return false
		}
	}
    date := time.Unix(liveSecond.Load(), 0).Format("Mon, 02 Jan 2006 15:04:05 GMT")
    buf = buf[:0]
    buf = append(buf, httpRespHeader...)
    buf = append(buf, []byte(date)...)
    buf = append(buf, httpRespContentLength...)
	netfd.Write(fd, buf) // Connection: close
	return false                      // will goto OnClose
}
func (h *Http) OnClose(fd int) {
	netfd.Close(fd)
}

type Https struct {
	Http
}
func updateLiveSecond() {
	for {
		select {
        case now := <-ticker.C:
			liveSecond.Store(now.Unix())
		}
	}
}

func main() {
	fmt.Println("hello boy")
    liveSecond.Store(time.Now().Unix())
    ticker = time.NewTicker(time.Millisecond * 1000)
    go updateLiveSecond()

    httpRespHeader = []byte(httpHeaderS)
    httpRespContentLength = []byte("\r\n" + contentLengthS)

	buffPool = &sync.Pool{
		New: func() any {
			return make([]byte, 1024)
		},
	}
	forAccept, err := goev.NewReactor(
		goev.EvDataArrSize(20480), // default val
		goev.EvPollNum(1),
		goev.EvReadyNum(8), // only accept fd
	)
	if err != nil {
		panic(err.Error())
	}
	forNewFd, err := goev.NewReactor(
		goev.EvDataArrSize(20480), // default val
		goev.EvPollNum(runtime.NumCPU() - 1),
		goev.EvReadyNum(512), // auto calc
	)
	if err != nil {
		panic(err.Error())
	}
	//= http
	_, err = goev.NewAcceptor(forAccept, forNewFd, func() goev.EvHandler { return new(Http) },
		":8080",
		goev.ListenBacklog(512),
		goev.RecvBuffSize(8*1024), // 短链接, 不需要很大的缓冲区
	)
	if err != nil {
		panic(err.Error())
	}

	go func() {
		if err = forAccept.Run(); err != nil {
			panic(err.Error())
		}
	}()
	if err = forNewFd.Run(); err != nil {
		panic(err.Error())
	}
}
