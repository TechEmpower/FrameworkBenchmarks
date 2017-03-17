package controllers

import "hello/models"

type QueriesController struct {
	Base
}

func (c *QueriesController) Get() {
	queries, _ := c.GetInt("queries")
	c.Data["json"] = models.GetQueriesWorld(queries)
	c.ServeJSON()
}
