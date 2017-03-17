package controllers

import "github.com/astaxie/beego"

type Base struct {
	beego.Controller
}

func (c *Base) Prepare() {
	c.Ctx.Output.Header("Server", "Beego")
}
