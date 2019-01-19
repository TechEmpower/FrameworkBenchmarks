package controllers

import "hello/models"

type JsonController struct {
	Base
}

func (c *JsonController) Get() {
	c.Data[json] = &models.Message{Message: helloWorldString}
	c.ServeJSON()
}
