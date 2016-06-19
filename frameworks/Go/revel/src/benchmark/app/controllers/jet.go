package controllers

import (
	"math/rand"
	"sort"

	"benchmark/app/db"

	"github.com/revel/revel"
)

func (c App) JetDb(queries int) revel.Result {
	if queries <= 1 {
		var w World
		err := db.Jet.Query(WorldSelect, rand.Intn(WorldRowCount)+1).Rows(&w)
		if err != nil {
			revel.ERROR.Fatalf("Db/WorldSelect error: %v", err)
		}
		return c.RenderJson(w)
	}

	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		err := db.Jet.Query(WorldSelect, rand.Intn(WorldRowCount)+1).Rows(&ww[i])
		if err != nil {
			revel.ERROR.Fatalf("Db/WorldSelect2 error: %v", err)
		}
	}
	return c.RenderJson(ww)
}

func (c App) JetUpdate(queries int) revel.Result {
	if queries <= 1 {
		var w World
		err := db.Jet.Query(WorldSelect, rand.Intn(WorldRowCount)+1).Rows(&w)
		if err != nil {
			revel.ERROR.Fatalf("Update/WorldSelect error: %v", err)
		}
		w.RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
		if err = db.Jet.Query(WorldUpdate, w.RandomNumber, w.Id).Run(); err != nil {
			revel.ERROR.Fatalf("Update/WorldUpdate error: %v", err)
		}
		return c.RenderJson(&w)
	}

	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		err := db.Jet.Query(WorldSelect, rand.Intn(WorldRowCount)+1).Rows(&ww[i])
		if err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
		ww[i].RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
		if err = db.Jet.Query(WorldUpdate, ww[i].RandomNumber, ww[i].Id).Run(); err != nil {
			revel.ERROR.Fatalf("Update/WorldUpdate2 error: %v", err)
		}
	}
	return c.RenderJson(ww)
}

func (c App) JetFortune() revel.Result {
	var fortunes Fortunes
	err := db.Jet.Query(FortuneSelect).Rows(&fortunes)
	if err != nil {
		revel.ERROR.Fatalf("Fortune/FortuneSelect error: %v", err)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
	sort.Sort(ByMessage{fortunes})
	c.RenderArgs["fortunes"] = fortunes
	return c.RenderTemplate("App/Fortune.html")
}
