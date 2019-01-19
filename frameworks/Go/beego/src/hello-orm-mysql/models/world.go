package models

import (
	"log"
	"math/rand"

	"github.com/astaxie/beego/orm"
)

type World struct {
	Id           uint16 `orm:"pk" json:"id"`
	RandomNumber uint16 `orm:"column(randomNumber)" json:"randomNumber"`
}

const WorldSelect = "SELECT id, randomNumber FROM World WHERE id = ?"
const WorldUpdate = "UPDATE World SET randomNumber = ? WHERE id = ?"
const WorldRowCount = 10000

func GetQueriesWorld(queries int) *[]World {
	o := orm.NewOrm()
	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		err := o.Raw(WorldSelect, rand.Intn(WorldRowCount)+1).QueryRow(&ww[i])
		if err != nil {
			log.Fatalf("Error scanning world row: %v", err)
		}
		ww[i].RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
		_, err = o.Raw(WorldUpdate, ww[i].RandomNumber, ww[i].Id).Exec()
	}
	return &ww
}
