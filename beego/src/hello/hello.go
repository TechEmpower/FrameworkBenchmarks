package main

import (
	"github.com/astaxie/beego"
	//"runtime"
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
	//don't need this set, beego default set it
	//runtime.GOMAXPROCS(runtime.NumCPU())
	beego.RunMode = "prod"
	beego.Router("/json", &JsonController{})
	beego.Run()
}
