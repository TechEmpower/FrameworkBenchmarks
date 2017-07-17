package controllers

import "github.com/astaxie/beego"

type Base struct {
	beego.Controller
}

const Server = "Server"
const Beego = "Beego"
const queries = "queries"
const json = "json"

func (c *Base) Prepare() {
	c.Ctx.Output.Header(Server, Beego)
}

func (c *Base) getQueriesParam() int {
	n, err := c.GetInt(queries)
	if err != nil || n < 1 {
		return 1
	}
	if n > 500 {
		return 500
	}
	return n
}
