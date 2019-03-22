package main

import (
	"log"
	"net"
	"os"
	"os/exec"
	"runtime"
)

func doPrefork(child bool, toBind string) (net.Listener, error) {
	var listener net.Listener
	if !child {
		addr, err := net.ResolveTCPAddr("tcp", toBind)
		if err != nil {
			return nil, err
		}
		tcplistener, err := net.ListenTCP("tcp", addr)
		if err != nil {
			return nil, err
		}
		fl, err := tcplistener.File()
		if err != nil {
			return nil, err
		}
		children := make([]*exec.Cmd, runtime.NumCPU())
		for i := range children {
			children[i] = exec.Command(os.Args[0], append(os.Args[1:], "-child")...)
			children[i].Stdout = os.Stdout
			children[i].Stderr = os.Stderr
			children[i].ExtraFiles = []*os.File{fl}
			if err := children[i].Start(); err != nil {
				return nil, err
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
			return nil, err
		}
		runtime.GOMAXPROCS(1)
	}
	return listener, nil
}
