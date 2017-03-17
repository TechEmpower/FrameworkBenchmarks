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

const worldSelect = "SELECT id, randomNumber FROM World WHERE id = ?"
const worldUpdate = "UPDATE World SET randomNumber = ? WHERE id = ?"
const worldRowCount = 10000

func GetQueriesWorld(queries int) *[]World{
	o := orm.NewOrm()
	if queries < 1 {
		queries = 1
	} else if queries > 500 {
		queries = 500
	}
	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		err := o.Raw(worldSelect, rand.Intn(worldRowCount)+1).QueryRow(&ww[i])
		if err != nil {
			log.Fatalf("Error scanning world row: %v", err)
		}
		ww[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		_, err = o.Raw(worldUpdate, ww[i].RandomNumber, ww[i].Id).Exec()
	}
	return &ww
}
