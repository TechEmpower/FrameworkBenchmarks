package main

import (
	"github.com/astaxie/beego"
	"runtime"
)

type MessageStruct struct {
	Message string
}

type JsonController struct {
	beego.Controller
}

func (this *JsonController) Get() {
	m := MessageStruct{"Hello, world"}
	this.Data["json"] = &m
	this.ServeJson()
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	beego.Router("/json", &JsonController{})
	beego.Run()
}
