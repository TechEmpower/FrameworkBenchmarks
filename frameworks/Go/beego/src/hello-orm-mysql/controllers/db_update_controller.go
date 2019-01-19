package controllers

import (
	"log"
	"math/rand"

	"hello-orm-mysql/models"

	"github.com/astaxie/beego/orm"
)

type DBUpdateController struct {
	Base
}

func (c *DBUpdateController) Get() {
	n := c.getQueriesParam()
	o := orm.NewOrm()
	world := make([]models.World, n)
	for i := 0; i < n; i++ {
		if err := o.Raw(models.WorldSelect, rand.Intn(models.WorldRowCount)+1).QueryRow(&world[i].Id, &world[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %v", err)
		}
		world[i].RandomNumber = uint16(rand.Intn(models.WorldRowCount) + 1)
		if _, err := o.Raw(models.WorldUpdate, world[i].RandomNumber, world[i].Id).Exec(); err != nil {
			log.Fatalf("Error updating world row: %v", err)
		}
	}
	c.Data[json] = &world
	c.ServeJSON()
}
