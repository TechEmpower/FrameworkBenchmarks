package controllers

import (
	"math/rand"
	"sort"

	"github.com/coocood/qbs"
	"github.com/revel/revel"
)

func (c App) QbsDb(queries int) revel.Result {
	qbs, _ := qbs.GetQbs()
	defer qbs.Close()

	if queries <= 1 {
		var w World
		w.Id = uint16(rand.Intn(WorldRowCount) + 1)
		err := qbs.Find(&w)
		if err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
		return c.RenderJSON(w)
	}

	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		ww[i].Id = uint16(rand.Intn(WorldRowCount) + 1)
		if err := qbs.Find(&ww[i]); err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
	}
	return c.RenderJSON(ww)
}

func (c App) QbsUpdate(queries int) revel.Result {
	qbs, _ := qbs.GetQbs()
	defer qbs.Close()

	if queries <= 1 {
		var w World
		w.Id = uint16(rand.Intn(WorldRowCount) + 1)
		if err := qbs.Find(&w); err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
		w.RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
		if _, err := qbs.Save(&w); err != nil {
			revel.ERROR.Fatalf("Error updating world row: %v", err)
		}
		return c.RenderJSON(&w)
	}

	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		ww[i].Id = uint16(rand.Intn(WorldRowCount) + 1)
		if err := qbs.Find(&ww[i]); err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
		ww[i].RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
		if _, err := qbs.Save(&ww[i]); err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
	}
	return c.RenderJSON(ww)
}

func (c App) QbsFortune() revel.Result {
	qbs, _ := qbs.GetQbs()
	defer qbs.Close()

	var fortunes []*Fortune
	qbs.FindAll(&fortunes)
	fortunes = append(fortunes,
		&Fortune{Message: "Additional fortune added at request time."})
	sort.Sort(ByMessage{fortunes})
	c.ViewArgs["fortunes"] = fortunes
	return c.RenderTemplate("App/Fortune.html")
}
