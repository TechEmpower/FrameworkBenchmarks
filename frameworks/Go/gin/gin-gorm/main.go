package main

import (
	"fmt"
	"math/rand"
	"net/http"
	"time"

	"github.com/gin-gonic/gin"
	postgres "gorm.io/driver/postgres"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
)

// World represents an entry int the World table
type World struct {
	ID           int64 `json:"id"`
	RandomNumber int64 `json:"randomNumber" gorm:"column:randomnumber"`
}

// Override GORM convention for table mapping
// TableName overrides the table name used by User to `profiles`
func (World) TableName() string {
	return "World"
}

// implements the basic logic behind the query tests
func getWorld(db *gorm.DB) World {
	//we could actually precompute a list of random
	//numbers and slice them but this makes no sense
	//as I expect that this 'random' is just a placeholder
	//for an actual business logic
	randomId := rand.Intn(10000) + 1

	var world World
	db.Take(&world, randomId)

	return world
}

// implements the logic behind the updates tests
func processWorld(tx *gorm.DB) (World, error) {
	//we could actually precompute a list of random
	//numbers and slice them but this makes no sense
	//as I expect that this 'random' is just a placeholder
	//for an actual business logic in a real test
	randomId := rand.Intn(10000) + 1
	randomId2 := int64(rand.Intn(10000) + 1)

	var world World
	tx.Take(&world, randomId)

	world.RandomNumber = randomId2
	err := tx.Save(&world).Error

	return world, err
}

func main() {
	/* SETUP DB AND WEB SERVER */

	dsn := "host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world port=5432 sslmode=disable"
	db, err := gorm.Open(postgres.Open(dsn), &gorm.Config{
		PrepareStmt: true,                                  // use prep statements
		Logger:      logger.Default.LogMode(logger.Silent), // new, not inserted in original submission 2x on query
	})

	if err != nil {
		panic("failed to connect database")
	}

	sqlDB, err := db.DB()
	if err != nil {
		panic("failed to get underlying db conn pooling struct")
	}

	// SetMaxIdleConns sets the maximum number of connections in the idle connection pool.
	sqlDB.SetMaxIdleConns(500)

	//r := gin.Default() // use default middleware - original submission
	r := gin.New() // use no middleware, causes 1.83x on plaintext (pure gin still at +14% on both plaintext and json)

	// setup middleware to add server header
	// this slows things a little bit but it is the best design decision
	serverHeader := []string{"Gin-gorm"}
	r.Use(func(c *gin.Context) {
		c.Writer.Header()["Server"] = serverHeader
	})

	/* START TESTS */

	// JSON TEST
	r.GET("/json", func(c *gin.Context) {
		//c.Header("Server", "example") - original submission now using middleware
		c.JSON(200, gin.H{"message": "Hello, World!"})
	})

	// PLAINTEXT TEST
	r.GET("/plaintext", func(c *gin.Context) {
		//c.Header("Server", "example") - original submission now using middleware
		c.String(200, "Hello, World!")
	})

	// SINGLE QUERY
	r.GET("/db", func(c *gin.Context) {
		world := getWorld(db)

		//c.Header("Server", "example") - original submission now using middleware
		c.JSON(200, world)
	})

	type NumOf struct {
		Queries int `form:"queries"`
	}

	// MULTIPLE QUERIES
	r.GET("/queries", func(c *gin.Context) {
		var numOf NumOf

		if c.ShouldBindQuery(&numOf) != nil { // manage missing query num
			numOf.Queries = 1

		} else if numOf.Queries < 1 { //set at least 1
			numOf.Queries = 1

		} else if numOf.Queries > 500 { //set no more than 500
			numOf.Queries = 500
		}

		worlds := make([]World, numOf.Queries, numOf.Queries) //prealloc

		//original submission with go routines, seems faster then without...
		channel := make(chan World, numOf.Queries)

		for i := 0; i < numOf.Queries; i++ {
			go func() { channel <- getWorld(db) }()
		}

		for i := 0; i < numOf.Queries; i++ {
			worlds[i] = <-channel
		}

		//c.Header("Server", "example") - original submission now using middleware
		c.JSON(200, worlds)
	})

	// MULTIPLE UPDATES
	r.GET("/updates", func(c *gin.Context) {
		var numOf NumOf

		if c.ShouldBindQuery(&numOf) != nil { // manage missing query num
			numOf.Queries = 1

		} else if numOf.Queries < 1 { //set at least 1
			numOf.Queries = 1

		} else if numOf.Queries > 500 { //set no more than 500
			numOf.Queries = 500
		}

		worlds := make([]World, numOf.Queries, numOf.Queries) //prealloc
		var err error = nil

		//c.Header("Server", "example") - original submission now using middleware

		for i := 0; i < numOf.Queries; i++ {
			worlds[i], err = processWorld(db)

			if err != nil {
				fmt.Println(err)
				c.JSON(500, gin.H{"error": err})
				break
			}
		}

		c.JSON(200, worlds)
	})

	/* START SERVICE */

	s := &http.Server{
		Addr:         ":8080",
		Handler:      r,
		ReadTimeout:  100000 * time.Second, //increase keepalive
		WriteTimeout: 100000 * time.Second,
	}

	s.ListenAndServe() // listen and serve on 0.0.0.0:8080
}
