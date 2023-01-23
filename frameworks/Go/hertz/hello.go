package main

import (
	"context"
	"fmt"
	"github.com/cloudwego/hertz/pkg/app"
	"github.com/cloudwego/hertz/pkg/app/server"
	"github.com/cloudwego/hertz/pkg/common/config"
	"github.com/cloudwego/hertz/pkg/common/utils"
	"log"
	"math/rand"
	"runtime"
	"sort"
	"strconv"

	"database/sql"

	_ "github.com/go-sql-driver/mysql"
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

func parseQueries(c context.Context, ctx *app.RequestContext) int {
	n, err := strconv.Atoi(ctx.Query("queries"))
	if err != nil {
		n = 1
	} else if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

// / Test 1: JSON serialization
func json(c context.Context, ctx *app.RequestContext) {
	ctx.JSON(200, utils.H{"message": "Hello, World!"})
}

// / Test 2: Single database query
func db(c context.Context, ctx *app.RequestContext) {
	var world World
	err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
	if err != nil {
		ctx.AbortWithError(500, err)
		return
	}
	ctx.JSON(200, &world)
}

// / Test 3: Multiple database queries
func dbs(c context.Context, ctx *app.RequestContext) {
	numQueries := parseQueries(c, ctx)

	worlds := make([]World, numQueries)
	for i := 0; i < numQueries; i++ {
		err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&worlds[i].Id, &worlds[i].RandomNumber)
		if err != nil {
			ctx.AbortWithError(500, err)
			return
		}
	}
	ctx.JSON(200, &worlds)
}

// / Test 4: Fortunes
func fortunes(c context.Context, ctx *app.RequestContext) {
	rows, err := fortuneStatement.Query()
	if err != nil {
		ctx.AbortWithError(500, err)
		return
	}

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() { //Fetch rows
		fortune := Fortune{}
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			ctx.AbortWithError(500, err)
			return
		}
		fortunes = append(fortunes, &fortune)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})
	ctx.HTML(200, "fortune.html", fortunes)
}

// / Test 5: Database updates
func update(c context.Context, ctx *app.RequestContext) {
	numQueries := parseQueries(c, ctx)
	world := make([]World, numQueries)
	for i := 0; i < numQueries; i++ {
		if err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber); err != nil {
			ctx.AbortWithError(500, err)
			return
		}
		world[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		if _, err := updateStatement.Exec(world[i].RandomNumber, world[i].Id); err != nil {
			ctx.AbortWithError(500, err)
			return
		}
	}
	ctx.JSON(200, world)
}

// / Test 6: plaintext
func plaintext(c context.Context, ctx *app.RequestContext) {
	ctx.String(200, "Hello, World!")
}

func main() {
	h := server.New(config.Option{F: func(o *config.Options) {
		o.Addr = ":8080"
	},
	})
	serverHeader := []string{"Hertz"}
	h.Use(func(c context.Context, ctx *app.RequestContext) {
		ctx.Header("Server", serverHeader[0])
	})
	h.LoadHTMLGlob("/templates/fortune.html")
	h.GET("/json", json)
	h.GET("/db", db)
	h.GET("/dbs", dbs)
	h.GET("/fortunes", fortunes)
	h.GET("/update", update)
	h.GET("/plaintext", plaintext)
	h.Spin()
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
