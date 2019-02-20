package main

import (
	"flag"
	"log"
	"net"
	"net/http"
	"os"
	"os/exec"
	"runtime"
)

type Message struct {
	Message string `json:"message"`
}

const (
	helloWorldString = "Hello, World!"
)

var (
	helloWorldBytes = []byte(helloWorldString)
)

var prefork = flag.Bool("prefork", false, "use prefork")
var child = flag.Bool("child", false, "is child proc")

func main() {
	var listener net.Listener
	flag.Parse()
	if !*prefork {
		runtime.GOMAXPROCS(runtime.NumCPU())
	} else {
		listener = doPrefork()
	}

	http.HandleFunc("/json", jsonHandler)
	http.HandleFunc("/plaintext", plaintextHandler)

	if !*prefork {
		http.ListenAndServe(":8080", nil)
	} else {
		http.Serve(listener, nil)
	}
}

func doPrefork() net.Listener {
	var listener net.Listener
	if !*child {
		addr, err := net.ResolveTCPAddr("tcp", ":8080")
		if err != nil {
			log.Fatal(err)
		}
		tcplistener, err := net.ListenTCP("tcp", addr)
		if err != nil {
			log.Fatal(err)
		}
		fl, err := tcplistener.File()
		if err != nil {
			log.Fatal(err)
		}
		children := make([]*exec.Cmd, runtime.NumCPU())
		for i := range children {
			children[i] = exec.Command(os.Args[0], append(os.Args[1:], "-child")...)
			children[i].Stdout = os.Stdout
			children[i].Stderr = os.Stderr
			children[i].ExtraFiles = []*os.File{fl}
			err = children[i].Start()
			if err != nil {
				log.Fatal(err)
			}
		}
		for _, ch := range children {
			if err := ch.Wait(); err != nil {
				log.Print(err)
			}
		}
		os.Exit(0)
	} else {
		var err error
		listener, err = net.FileListener(os.NewFile(3, ""))
		if err != nil {
			log.Fatal(err)
		}
		runtime.GOMAXPROCS(1)

	}
	return listener
}
