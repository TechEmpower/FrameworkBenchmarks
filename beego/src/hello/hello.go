package main

import (
	"github.com/astaxie/beego"
	//"runtime"
)

type MessageStruct struct {
	message string `json:"message"`
}

type JsonController struct {
	beego.Controller
}

func (this *JsonController) Get() {
	m := MessageStruct{message: "Hello, World!"}
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
