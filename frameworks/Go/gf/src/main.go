package main

import (
	"github.com/gogf/gf/g"
	"github.com/gogf/gf/g/net/ghttp"
)

func main() {
	s := g.Server()
	s.BindHandler("/plaintext", func(r *ghttp.Request) {
		r.Response.Write("Hello, World!")
	})
	s.Run()
}
