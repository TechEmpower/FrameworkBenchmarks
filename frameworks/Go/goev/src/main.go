package main

import (
	"fmt"
    "time"
    "bytes"
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
    liveDate atomic.Value
)

const httpHeaderS = "HTTP/1.1 200 OK\r\nConnection: keep-alive\r\nServer: goev\r\nContent-Type: text/plain\r\nDate: "
const contentLengthS = "\r\nContent-Length: 13\r\n\r\nHello, World!"

type Http struct {
	goev.Event
}

func (h *Http) OnOpen(fd int, now int64) bool {
    netfd.SetNoDelay(fd, 1)
    // AddEvHandler 尽量放在最后, (OnOpen 和ORead可能不在一个线程)
	if err := h.GetReactor().AddEvHandler(h, fd, goev.EV_IN); err != nil {
		return false
	}
	return true
}
func (h *Http) OnRead(fd int, evPollSharedBuff []byte, now int64) bool {
    buf := evPollSharedBuff[:]
	readN := 0
	for {
		if readN >= cap(buf) { // alloc new buff to read
            readN = 1 // ^_^
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
            break // ^_^
		} else { // n == 0 connection closed,  will not < 0
			if readN == 0 {
				// fmt.Println("peer closed. ", n)
			}
			return false
		}
	}
	if idx := bytes.Index(buf, []byte{'\r', '\n', '\r', '\n'}); idx == -1 {
        return false
    }
    buf = buf[:0]
    buf = append(buf, httpRespHeader...)
    buf = append(buf, []byte(liveDate.Load().(string))...)
    buf = append(buf, httpRespContentLength...)
	netfd.Write(fd, buf) // Connection: close
	return true
}
func (h *Http) OnClose(fd int) {
	netfd.Close(fd)
}

func updateLiveSecond() {
	for {
		select {
        case now := <-ticker.C:
			liveDate.Store(now.Format("Mon, 02 Jan 2006 15:04:05 GMT"))
		}
	}
}

func main() {
	fmt.Println("hello boy")
    runtime.GOMAXPROCS(runtime.NumCPU() * 2)

    liveDate.Store(time.Now().Format("Mon, 02 Jan 2006 15:04:05 GMT"))
    ticker = time.NewTicker(time.Millisecond * 1000)

    httpRespHeader = []byte(httpHeaderS)
    httpRespContentLength = []byte(contentLengthS)

	forAcceptReactor, err := goev.NewReactor(
		goev.EvDataArrSize(20480), // default val
		goev.EvPollNum(1),
		goev.EvReadyNum(8), // only accept fd
		goev.NoTimer(true),
	)
	if err != nil {
		panic(err.Error())
	}
    cpuN := runtime.NumCPU()
    if cpuN > 7 {
        cpuN = cpuN * 3 / 2
    }
	forNewFdReactor, err := goev.NewReactor(
		goev.EvDataArrSize(20480), // default val
		goev.EvPollNum(cpuN),
		goev.EvReadyNum(128), // auto calc
		goev.NoTimer(true),
	)
	if err != nil {
		panic(err.Error())
	}
	//= http
	_, err = goev.NewAcceptor(forAcceptReactor, forNewFdReactor, func() goev.EvHandler { return new(Http) },
		":8080",
		goev.ListenBacklog(512),
		//goev.SockRcvBufSize(16*1024), // 短链接, 不需要很大的缓冲区
	)
	if err != nil {
		panic(err.Error())
	}

    go updateLiveSecond()
	go func() {
		if err = forAcceptReactor.Run(); err != nil {
			panic(err.Error())
		}
	}()
	if err = forNewFdReactor.Run(); err != nil {
		panic(err.Error())
	}
}
