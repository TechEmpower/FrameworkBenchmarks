package main

import (
	"hello/controllers"

	"github.com/astaxie/beego"
)

func main() {
	beego.BConfig.RunMode = "prod"
	beego.Router("/json", &controllers.JsonController{})
	beego.Router("/db", &controllers.DBController{})
	beego.Router("/plaintext", &controllers.PlaintextController{})
	beego.Router("/queries", &controllers.QueriesController{})
	beego.Run()
}
