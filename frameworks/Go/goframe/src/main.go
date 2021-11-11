package main

import (
	"log"
	"runtime"
	"sort"

	"database/sql"

	_ "github.com/go-sql-driver/mysql"

	"github.com/gogf/gf/frame/g"
	"github.com/gogf/gf/net/ghttp"
	"github.com/gogf/gf/util/grand"
)

type (
	World struct {
		Id           uint16 `json:"id"`
		RandomNumber uint16 `json:"randomNumber"`
	}

	Fortune struct {
		Id      uint16 `json:"id"`
		Message string `json:"message"`
	}

	Fortunes []*Fortune

	ByMessage struct {
		Fortunes
	}
)

func (s Fortunes) Len() int {
	return len(s)
}

func (s Fortunes) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func (s ByMessage) Less(i, j int) bool {
	return s.Fortunes[i].Message < s.Fortunes[j].Message
}

const (
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate        = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256
)

var (
	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt
)

func init() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	db, err := sql.Open(
		"mysql",
		"benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world",
	)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(maxConnectionCount)
	if worldStatement, err = db.Prepare(worldSelect); err != nil {
		log.Fatal(err)
	}
	if fortuneStatement, err = db.Prepare(fortuneSelect); err != nil {
		log.Fatal(err)
	}
	if updateStatement, err = db.Prepare(worldUpdate); err != nil {
		log.Fatal(err)
	}
}

func main() {
	g.View().SetAutoEncode(true)
	g.View().Parse("fortune.html", g.Map{"list":nil})
	s := g.Server()
	s.Group("/", func(group *ghttp.RouterGroup) {
		group.Middleware(func(r *ghttp.Request) {
			r.Middleware.Next()
			r.Response.Header().Set("Server", "GoFrame")
		})
		group.GET("/db", handlerDb)
		group.GET("/dbs", handlerDbs)
		group.GET("/json", handlerJson)
		group.GET("/update", handlerUpdate)
		group.GET("/fortunes", handlerFortunes)
		group.GET("/plaintext", handlerPlaintext)
	})
	s.SetPort(8080)
	s.Run()
}

/// Test 1: JSON serialization
func handlerJson(r *ghttp.Request) {
	r.Response.WriteJson(g.Map{
		"message": "Hello, World!",
	})
}

/// Test 2: Single database query
func handlerDb(r *ghttp.Request) {
	var world World
	err := worldStatement.QueryRow(grand.Intn(worldRowCount)+1).
		Scan(
			&world.Id,
			&world.RandomNumber,
		)
	if err != nil {
		r.Response.WriteStatusExit(500, err.Error())
	}
	r.Response.WriteJson(world)
}

/// Test 3: Multiple database queries
func handlerDbs(r *ghttp.Request) {
	var (
		queries = parseQueries(r)
		worlds  = make([]World, queries)
	)
	for i := 0; i < queries; i++ {
		err := worldStatement.QueryRow(grand.Intn(worldRowCount)+1).
			Scan(
				&worlds[i].Id,
				&worlds[i].RandomNumber,
			)
		if err != nil {
			r.Response.WriteStatusExit(500, err.Error())
		}
	}
	r.Response.WriteJson(worlds)
}

/// Test 4: Fortunes
func handlerFortunes(r *ghttp.Request) {
	rows, err := fortuneStatement.Query()
	if err != nil {
		r.Response.WriteStatusExit(500, err.Error())
	}
	fortunes := make(Fortunes, 0, 16)
	for rows.Next() {
		fortune := Fortune{}
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			r.Response.WriteStatusExit(500, err.Error())
		}
		fortunes = append(fortunes, &fortune)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
	sort.Sort(ByMessage{fortunes})

	r.Response.WriteTpl("fortune.html", g.Map{
		"list": fortunes,
	})
}

/// Test 5: Database updates
func handlerUpdate(r *ghttp.Request) {
	var (
		queries = parseQueries(r)
		world   = make([]World, queries)
	)
	for i := 0; i < queries; i++ {
		if err := worldStatement.QueryRow(grand.Intn(worldRowCount)+1).Scan(
			&world[i].Id,
			&world[i].RandomNumber,
		); err != nil {
			r.Response.WriteStatusExit(500, err.Error())
		}
		world[i].RandomNumber = uint16(grand.Intn(worldRowCount) + 1)
		if _, err := updateStatement.Exec(world[i].RandomNumber, world[i].Id); err != nil {
			r.Response.WriteStatusExit(500, err.Error())
		}
	}
	r.Response.WriteJson(world)
}

/// Test 6: plaintext
func handlerPlaintext(r *ghttp.Request) {
	r.Response.Write("Hello, World!")
}

func parseQueries(r *ghttp.Request) int {
	n := r.GetQueryInt("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}
