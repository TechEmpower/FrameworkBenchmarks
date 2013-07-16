package controllers

import (
	"benchmark/app/db"
	"github.com/coocood/qbs"
	"github.com/robfig/revel"
	"math/rand"
	"runtime"
	"sort"
)

type MessageStruct struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `json:"id" qbs:"pk"`
	RandomNumber uint16 `json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `json:"id" qbs:"pk"`
	Message string `json:"message"`
}

const (
	WorldRowCount      = 10000
	MaxConnectionCount = 256
)

func init() {
	revel.Filters = []revel.Filter{
		revel.RouterFilter,
		revel.ParamsFilter,
		revel.ActionInvoker,
	}
	revel.OnAppStart(func() {
		runtime.GOMAXPROCS(runtime.NumCPU())
		db.Init()
		qbs.ChangePoolSize(MaxConnectionCount)
	})
}

type App struct {
	*revel.Controller
}

func (c App) Db(queries int) revel.Result {
	qbs, _ := qbs.GetQbs()
	defer qbs.Close()

	if queries <= 1 {
		var w World
		w.Id = uint16(rand.Intn(WorldRowCount) + 1)
		err := qbs.Find(&w)
		if err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
		return c.RenderJson(w)
	}

	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		ww[i].Id = uint16(rand.Intn(WorldRowCount) + 1)
		if err := qbs.Find(&ww[i]); err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
	}
	return c.RenderJson(ww)
}

func (c App) Update(queries int) revel.Result {
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
		return c.RenderJson(&w)
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
	return c.RenderJson(ww)
}

func (c App) Fortune() revel.Result {
	qbs, _ := qbs.GetQbs()
	defer qbs.Close()

	var fortunes []*Fortune
	qbs.FindAll(&fortunes)
	fortunes = append(fortunes,
		&Fortune{Message: "Additional fortune added at request time."})
	sort.Sort(ByMessage{fortunes})
	return c.Render(fortunes)
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
