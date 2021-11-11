package main

import (
	"log"
	"net"
	"os"
	"os/exec"
	"runtime"
)

func doPrefork(isChild bool, bind string) (listener net.Listener) {
	var err error
	var fl *os.File
	var tcplistener *net.TCPListener
	if !isChild {
		var addr *net.TCPAddr
		addr, err = net.ResolveTCPAddr("tcp", bind)
		if err != nil {
			log.Fatal(err)
		}
		tcplistener, err = net.ListenTCP("tcp", addr)
		if err != nil {
			log.Fatal(err)
		}
		fl, err = tcplistener.File()
		if err != nil {
			log.Fatal(err)
		}
		children := make([]*exec.Cmd, runtime.NumCPU()/2)
		for i := range children {
			args := make([]string, len(os.Args)-1)
			copy(args, os.Args[1:])
			args = append(args, "-child")

			children[i] = exec.Command(os.Args[0], args...)
			children[i].Stdout = os.Stdout
			children[i].Stderr = os.Stderr
			children[i].ExtraFiles = []*os.File{fl}
			err = children[i].Start()
			if err != nil {
				log.Fatal(err)
			}
		}
		for _, ch := range children {
			err := ch.Wait()
			if err != nil {
				log.Print(err)
			}
		}
		os.Exit(0)
	} else {
		fl = os.NewFile(3, "")
		listener, err = net.FileListener(fl)
		if err != nil {
			log.Fatal(err)
		}
		runtime.GOMAXPROCS(runtime.NumCPU() / 2)
	}
	return listener
}
