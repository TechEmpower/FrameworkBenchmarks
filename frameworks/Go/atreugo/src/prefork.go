package main

import (
	"net"
	"os"
	"os/exec"
	"runtime"
)

func doPrefork(toBind string) error {
	addr, err := net.ResolveTCPAddr("tcp", toBind)
	if err != nil {
		return err
	}
	tcplistener, err := net.ListenTCP("tcp", addr)
	if err != nil {
		return err
	}
	fl, err := tcplistener.File()
	if err != nil {
		return err
	}

	children := make([]*exec.Cmd, runtime.NumCPU())
	for i := range children {
		children[i] = exec.Command(os.Args[0], append(os.Args[1:], "-child")...)
		children[i].Stdout = os.Stdout
		children[i].Stderr = os.Stderr
		children[i].ExtraFiles = []*os.File{fl}
		if err := children[i].Start(); err != nil {
			return err
		}
	}
	for _, ch := range children {
		if err := ch.Wait(); err != nil {
			return err
		}
	}

	return nil
}
