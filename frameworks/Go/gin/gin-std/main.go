package main

import (
	"fmt"
	"log"
	"math/rand"
	"runtime"
	"sort"
	"strconv"

	"database/sql"

	"github.com/gin-gonic/gin"
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

func parseQueries(c *gin.Context) int {
	n, err := strconv.Atoi(c.Request.URL.Query().Get("queries"))
	if err != nil {
		n = 1
	} else if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

/// Test 1: JSON serialization
func json(c *gin.Context) {
	c.JSON(200, gin.H{"message": "Hello, World!"})
}

/// Test 2: Single database query
func db(c *gin.Context) {
	var world World
	err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
	if err != nil {
		c.AbortWithError(500, err)
		return
	}
	c.JSON(200, &world)
}

/// Test 3: Multiple database queries
func dbs(c *gin.Context) {
	numQueries := parseQueries(c)

	worlds := make([]World, numQueries)
	for i := 0; i < numQueries; i++ {
		err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&worlds[i].Id, &worlds[i].RandomNumber)
		if err != nil {
			c.AbortWithError(500, err)
			return
		}
	}
	c.JSON(200, &worlds)
}

/// Test 4: Fortunes
func fortunes(c *gin.Context) {
	rows, err := fortuneStatement.Query()
	if err != nil {
		c.AbortWithError(500, err)
		return
	}

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() { //Fetch rows
		fortune := Fortune{}
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			c.AbortWithError(500, err)
			return
		}
		fortunes = append(fortunes, &fortune)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})
	c.HTML(200, "fortune.html", fortunes)
}

/// Test 5: Database updates
func update(c *gin.Context) {
	numQueries := parseQueries(c)
	world := make([]World, numQueries)
	for i := 0; i < numQueries; i++ {
		if err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber); err != nil {
			c.AbortWithError(500, err)
			return
		}
		world[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		if _, err := updateStatement.Exec(world[i].RandomNumber, world[i].Id); err != nil {
			c.AbortWithError(500, err)
			return
		}
	}
	c.JSON(200, world)
}

/// Test 6: plaintext
func plaintext(c *gin.Context) {
	c.String(200, "Hello, World!")
}

func main() {
	gin.SetMode(gin.ReleaseMode)

	r := gin.New()
	serverHeader := []string{"Gin"}
	r.Use(func(c *gin.Context) {
		c.Writer.Header()["Server"] = serverHeader
	})
	r.LoadHTMLGlob("templates/fortune.html")
	r.GET("/json", json)
	r.GET("/db", db)
	r.GET("/dbs", dbs)
	r.GET("/fortunes", fortunes)
	r.GET("/update", update)
	r.GET("/plaintext", plaintext)
	log.Print("Listening and serving HTTP\n")
	r.Run(":8080")
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
