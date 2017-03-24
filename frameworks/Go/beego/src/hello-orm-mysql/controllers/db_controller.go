package controllers

import (
	"log"
	"math/rand"

	"hello-orm-mysql/models"

	"github.com/astaxie/beego/orm"
)

const worldRowCount = 10000

type DBController struct {
	Base
}

func (c *DBController) Get() {
	o := orm.NewOrm()
	w := models.World{Id: uint16(rand.Intn(worldRowCount) + 1)}
	err := o.Read(&w)
	if err != nil {
		log.Fatalf("Error read world row: %s", err.Error())
	}
	c.Data[json] = &w
	c.ServeJSON()
}
