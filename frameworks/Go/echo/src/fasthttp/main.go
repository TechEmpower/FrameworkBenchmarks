package main

import (
	"common"
	"flag"
	"log"
	"net"
	"os"
	"os/exec"
	"runtime"

	"github.com/labstack/echo"
	"github.com/labstack/echo/engine"
	"github.com/labstack/echo/engine/fasthttp"
	"github.com/valyala/fasthttp/reuseport"
)

var (
	prefork = flag.Bool("prefork", false, "use prefork")
	child   = flag.Bool("child", false, "is child proc")
)

func getListener() net.Listener {
	if !*prefork {
		runtime.GOMAXPROCS(runtime.NumCPU())
		ln, err := net.Listen("tcp4", ":8080")
		if err != nil {
			log.Fatal(err)
		}
		return ln
	}

	if !*child {
		children := make([]*exec.Cmd, runtime.NumCPU())
		for i := range children {
			children[i] = exec.Command(os.Args[0], "-prefork", "-child")
			children[i].Stdout = os.Stdout
			children[i].Stderr = os.Stderr
			if err := children[i].Start(); err != nil {
				log.Fatal(err)
			}
		}
		for _, ch := range children {
			if err := ch.Wait(); err != nil {
				log.Print(err)
			}
		}
		os.Exit(0)
		panic("unreachable")
	}

	runtime.GOMAXPROCS(1)
	ln, err := reuseport.Listen("tcp4", ":8080")
	if err != nil {
		log.Fatal(err)
	}
	return ln
}

func main() {
	flag.Parse()
	e := echo.New()
	e.SetRenderer(common.Template)
	common.InitRoutes(e)
	common.InitPostgres()
	e.Run(fasthttp.NewFromConfig(engine.Config{
		Listener: getListener(),
	}))
}
