package controllers

import "github.com/astaxie/beego"

type Base struct {
	beego.Controller
}

const Server = "Server"
const Beego = "Beego"
const json = "json"

func (c *Base) Prepare() {
	c.Ctx.Output.Header(Server, Beego)
}
