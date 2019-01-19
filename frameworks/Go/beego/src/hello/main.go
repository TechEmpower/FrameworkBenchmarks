package main

import (
	"hello/controllers"

	"github.com/astaxie/beego"
)

func main() {
	beego.BConfig.RunMode = "prod"
	beego.Router("/json", &controllers.JsonController{})
	beego.Router("/plaintext", &controllers.PlaintextController{})
	beego.Run()
}
