package main

import (
	"database/sql"
	"fmt"
	_ "github.com/go-sql-driver/mysql"
	"github.com/xiusin/pine"
	"github.com/xiusin/pine/render/engine/template"
	"log"
	"math/rand"
	"runtime"
	"sort"
)


const (
	// Database
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate        = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256
)

type World struct {
	Id           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `json:"id"`
	Message string `json:"message"`
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }

var (
	// Database
	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt
)

func parseQueries(c *pine.Context) int {
	n, err := c.GetInt("queries")
	if err != nil {
		n = 1
	} else if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

func main() {
	app := pine.New()
	pine.RegisterViewEngine(template.New("templates", ".html", false))
	app.GET("/json", func(ctx *pine.Context) {
		ctx.Render().JSON(pine.H{"message": "Hello, World!"})
	})
	app.GET("/db", func(ctx *pine.Context) {
		var world World
		err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
		if err != nil {
			ctx.Abort(500, err.Error())
			return
		}
		ctx.Render().JSON(&world)
	})
	app.GET("/dbs", func(c *pine.Context) {
		numQueries := parseQueries(c)

		worlds := make([]World, numQueries)
		for i := 0; i < numQueries; i++ {
			err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&worlds[i].Id, &worlds[i].RandomNumber)
			if err != nil {
				c.Abort(500, err.Error())
				return
			}
		}
		c.Render().JSON(&worlds)
	})
	
	app.GET("/fortunes", func(c *pine.Context) {
		rows, err := fortuneStatement.Query()
		if err != nil {
			c.Abort(500, err.Error())
			return
		}

		fortunes := make(Fortunes, 0, 16)
		for rows.Next() { //Fetch rows
			fortune := Fortune{}
			if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
				c.Abort(500, err.Error())
				return
			}
			fortunes = append(fortunes, &fortune)
		}
		fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
		sort.Sort(ByMessage{fortunes})
		c.Render().ViewData("fortunes", fortunes)
		c.Render().HTML( "fortune.html")
	})
	
	app.GET("/plaintext", func(ctx *pine.Context) {
		ctx.WriteString("Hello, World!")
	})
	app.Run(pine.Addr(":8080"), pine.WithCharset("UTF-8"), pine.WithServerName("pine"))
}


func init() {
	runtime.GOMAXPROCS(runtime.NumCPU())

	dsn := "benchmarkdbuser:benchmarkdbpass@tcp(%s:3306)/hello_world"
	dbhost := "tfb-database"

	db, err := sql.Open("mysql", fmt.Sprintf(dsn, dbhost))
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(maxConnectionCount)
	worldStatement, err = db.Prepare(worldSelect)
	if err != nil {
		log.Fatal(err)
	}
	fortuneStatement, err = db.Prepare(fortuneSelect)
	if err != nil {
		log.Fatal(err)
	}
	updateStatement, err = db.Prepare(worldUpdate)
	if err != nil {
		log.Fatal(err)
	}
}
