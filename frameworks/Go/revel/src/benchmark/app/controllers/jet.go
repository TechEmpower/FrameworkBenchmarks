package controllers

import (
	"math/rand"
	"sort"

	"benchmark/app/db"

	"github.com/revel/revel"
)

func (c App) JetDb(queries int) revel.Result {
        _, foundQuery := c.Params.Values["queries"]
        if queries>500 {
             queries = 500
        }
        if queries == 0 {
          queries = 1
        }

	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		err := db.Jet.Query(WorldSelect, rand.Intn(WorldRowCount)+1).Rows(&ww[i])
		if err != nil {
			revel.ERROR.Fatalf("Db/WorldSelect2 error: %v", err)
		}
	}
        if !foundQuery {
            return c.RenderJSON(ww[0])
        }
        return c.RenderJSON(ww)
}

func (c App) JetUpdate(queries int) revel.Result {
        _, foundQuery := c.Params.Values["queries"]
        if queries>500 {
             queries = 500
        }
        if queries == 0 {
          queries = 1
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
        if !foundQuery {
            return c.RenderJSON(ww[0])
        }
        return c.RenderJSON(ww)
}

func (c App) JetFortune() revel.Result {
	var fortunes Fortunes
	err := db.Jet.Query(FortuneSelect).Rows(&fortunes)
	if err != nil {
		revel.ERROR.Fatalf("Fortune/FortuneSelect error: %v", err)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
	sort.Sort(ByMessage{fortunes})
	c.ViewArgs["fortunes"] = fortunes
	return c.RenderTemplate("App/Fortune.html")
}
