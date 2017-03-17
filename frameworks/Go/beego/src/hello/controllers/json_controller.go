package controllers

import "hello/models"

type JsonController struct {
	Base
}

func (c *JsonController) Get() {
	m := models.Message{Message: "Hello, World!"}
	c.Data["json"] = &m
	c.ServeJSON()
}
