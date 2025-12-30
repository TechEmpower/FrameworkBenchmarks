package main

import (
        "context"
        "bytes"
        "log"
        "net"
        "os"
        "os/exec"
        "runtime"
        "syscall"
        "github.com/cloudwego/hertz/pkg/app"
        "github.com/cloudwego/hertz/pkg/app/server"
        cxcputhread "github.com/cloudxaas/gocpu/thread"
        cxstrconv "github.com/cloudxaas/gostrconv"
        "github.com/panjf2000/ants/v2"
        "golang.org/x/sys/unix"
)

func main() {
        var err error

        if cxcputhread.CPUThread == 0 {

                childs := uint16(runtime.GOMAXPROCS(-1)) // Start a child for each CPU.
                ids := make(chan uint16, childs)

                var i uint16
                for i = 0; i < childs; i++ {
                        ids <- i + 1
                }

                for id := range ids {
                        idP := id
                        ants.Submit(func() {
                                cmd := exec.Command(os.Args[0], append(os.Args[1:], "-t="+cxstrconv.Uint16toa(idP))...)
                                cmd.Stdout = os.Stdout
                                cmd.Stderr = os.Stderr

                                log.Printf("id: %d", idP)
                                err := cmd.Run()
                                //time.Sleep(1*time.Second)
                                log.Printf("%s %s child %d exited with error: %v\n", os.Args[0], os.Args[1:], idP, err)
                                //time.Sleep(100*time.Second)
                                ids <- idP // When a child dies we just restart it.
                        })

                }

        }

        runtime.GOMAXPROCS(1)
        err = cxcputhread.SetCPUAffinity(cxcputhread.CPUThread)
        if err != nil {
                panic(err)
        }
        h := server.New(

                server.WithHostPorts("0.0.0.0:8080"),
                ///*
                server.WithListenConfig(&net.ListenConfig{
                        Control: func(network, address string, c syscall.RawConn) error {

                                return c.Control(func(fd uintptr) {
                                        syscall.SetsockoptInt(int(fd), syscall.SOL_SOCKET, unix.SO_REUSEPORT, 1)
                                })

                        },
                }),
                //*/
        )

/*
        h.GET("/plaintext", func(ctx context.Context, c *app.RequestContext) {
                c.String(consts.StatusOK, "get")
        })
*/
///*
        h.Use(func(cc context.Context, ctx *app.RequestContext) {
                HertzHandler(&cc, ctx)
        })
//*/
        h.Spin()

}

func HertzHandler(cc *context.Context, ctx *app.RequestContext) {
        if bytes.EqualFold(ctx.Request.URI().RequestURI(), []byte("/plaintext")) {
                ctx.Response.Header.SetStatusCode(200)
                ctx.Response.Header.SetContentType("text/plain")
                ctx.Response.SetBodyRaw([]byte("Hello World!"))
        }
        if bytes.EqualFold(ctx.Request.URI().RequestURI(), []byte("/json")) {
                ctx.Response.Header.SetStatusCode(200)
                ctx.Response.Header.SetContentType("text/json")
                ctx.Response.SetBodyRaw([]byte("{\"Hello World!\"}"))
        }
}
