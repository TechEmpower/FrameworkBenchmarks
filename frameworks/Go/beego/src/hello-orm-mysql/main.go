package main

import (
	"hello-orm-mysql/controllers"

	"github.com/astaxie/beego"
)

func main() {
	beego.BConfig.RunMode = "prod"
	beego.Router("/db", &controllers.DBController{})
	beego.Router("/update", &controllers.DBUpdateController{})
	beego.Router("/queries", &controllers.QueriesController{})
	beego.Run()
}
