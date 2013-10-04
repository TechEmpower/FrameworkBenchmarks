package controllers

import (
	"database/sql"
	"github.com/robfig/revel"
	"github.com/robfig/revel/modules/db/app"
	"math/rand"
	"runtime"
	"sort"
)

type MessageStruct struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `json:"id"`
	Message string `json:"message"`
}

const (
	WorldSelect        = "SELECT id,randomNumber FROM World where id=?"
	FortuneSelect      = "SELECT id,message FROM Fortune"
	WorldUpdate        = "UPDATE World SET randomNumber = ? where id = ?"
	WorldRowCount      = 10000
	MaxConnectionCount = 256
)

var (
	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt
)

func init() {
	revel.Filters = []revel.Filter{
		revel.RouterFilter,
		revel.ParamsFilter,
		revel.ActionInvoker,
	}
	revel.OnAppStart(func() {
		var err error
		runtime.GOMAXPROCS(runtime.NumCPU())
		db.Init()
		db.Db.SetMaxIdleConns(MaxConnectionCount)
		if worldStatement, err = db.Db.Prepare(WorldSelect); err != nil {
			revel.ERROR.Fatalln(err)
		}
		if fortuneStatement, err = db.Db.Prepare(FortuneSelect); err != nil {
			revel.ERROR.Fatalln(err)
		}
		if updateStatement, err = db.Db.Prepare(WorldUpdate); err != nil {
			revel.ERROR.Fatalln(err)
		}
	})
}

type App struct {
	*revel.Controller
}

func (c App) Json() revel.Result {
	return c.RenderJson(MessageStruct{"Hello, world"})
}

func (c App) Plaintext() revel.Result {
	return c.RenderText("Hello, World!")
}

func (c App) Db(queries int) revel.Result {
	if queries <= 1 {
		var w World
		err := worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).
			Scan(&w.Id, &w.RandomNumber)
		if err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
		return c.RenderJson(w)
	}

	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		err := worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).
			Scan(&ww[i].Id, &ww[i].RandomNumber)
		if err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
	}
	return c.RenderJson(ww)
}

func (c App) Update(queries int) revel.Result {
	if queries <= 1 {
		var w World
		err := worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).
			Scan(&w.Id, &w.RandomNumber)
		if err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
		w.RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
		_, err = updateStatement.Exec(w.RandomNumber, w.Id)
		if err != nil {
			revel.ERROR.Fatalf("Error updating row: %v", err)
		}
		return c.RenderJson(&w)
	}

	ww := make([]World, queries)
	for i := 0; i < queries; i++ {
		err := worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).
			Scan(&ww[i].Id, &ww[i].RandomNumber)
		if err != nil {
			revel.ERROR.Fatalf("Error scanning world row: %v", err)
		}
		ww[i].RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
		updateStatement.Exec(ww[i].RandomNumber, ww[i].Id)
	}
	return c.RenderJson(ww)
}

func (c App) Fortune() revel.Result {
	fortunes := make([]*Fortune, 0, 16)

	rows, err := fortuneStatement.Query()
	if err != nil {
		revel.ERROR.Fatalf("Error preparing statement: %v", err)
	}

	i := 0
	var fortune *Fortune
	for rows.Next() {
		fortune = new(Fortune)
		if err = rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			revel.ERROR.Fatalf("Error scanning fortune row: %v", err)
		}
		fortunes = append(fortunes, fortune)
		i++
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})
	return c.Render(fortunes)
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
