package controllers

import "hello-orm-mysql/models"

type QueriesController struct {
	Base
}

func (c *QueriesController) Get() {
	n := c.getQueriesParam()
	c.Data[json] = models.GetQueriesWorld(n)
	c.ServeJSON()
}
