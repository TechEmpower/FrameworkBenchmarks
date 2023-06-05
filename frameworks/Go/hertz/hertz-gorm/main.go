package main

import (
	"context"
	"fmt"
	"github.com/cloudwego/hertz/pkg/app"
	"github.com/cloudwego/hertz/pkg/app/server"
	"github.com/cloudwego/hertz/pkg/common/config"
	"github.com/cloudwego/hertz/pkg/common/utils"
	postgres "gorm.io/driver/postgres"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
	"math/rand"
)

// World represents an entry int the World table
type World struct {
	ID           int64 `json:"id"`
	RandomNumber int64 `json:"randomNumber" gorm:"column:randomnumber"`
}

// TableName Override GORM convention for table mapping
func (World) TableName() string {
	return "World"
}

// implements the basic logic behind the query tests
func getWorld(db *gorm.DB) World {
	// we could actually precompute a list of random
	// numbers and slice them but this makes no sense
	// as I expect that this 'random' is just a placeholder
	// for an actual business logic
	randomId := rand.Intn(10000) + 1

	var world World
	db.Take(&world, randomId)

	return world
}

// implements the logic behind the updates tests
func processWorld(tx *gorm.DB) (World, error) {
	// we could actually precompute a list of random
	// numbers and slice them but this makes no sense
	// as I expect that this 'random' is just a placeholder
	// for an actual business logic in a real test
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

	h := server.New(config.Option{F: func(o *config.Options) {
		o.Addr = ":8080"
	}}) // use default middleware

	// setup middleware to add server header
	// this slows things a little, but it is the best design decision
	serverHeader := []string{"Hertz-gorm"}
	h.Use(func(c context.Context, ctx *app.RequestContext) {
		ctx.Header("Server", serverHeader[0])
	})

	/* START TESTS */
	// JSON TEST
	h.GET("/json", func(c context.Context, ctx *app.RequestContext) {
		// ctx.Header("Server", "example") - original submission now using middleware
		ctx.JSON(200, utils.H{"message": "Hello, World!"})
	})
	// PLAINTEXT TEST
	h.GET("/plaintext", func(c context.Context, ctx *app.RequestContext) {
		// ctx.Header("Server", "example") - original submission now using middleware
		ctx.String(200, "Hello, World!")
	})
	// SINGLE QUERY
	h.GET("/db", func(c context.Context, ctx *app.RequestContext) {
		world := getWorld(db)
		// ctx.Header("Server", "example") - original submission now using middleware
		ctx.JSON(200, world)
	})

	type NumOf struct {
		Queries int `form:"queries" query:"queries"`
	}
	// MULTIPLE QUERIES
	h.GET("/queries", func(c context.Context, ctx *app.RequestContext) {
		var numOf NumOf
		if ctx.Bind(&numOf) != nil { // manage missing query num
			numOf.Queries = 1
		} else if numOf.Queries < 1 { // set at least 1
			numOf.Queries = 1
		} else if numOf.Queries > 500 { // set no more than 500
			numOf.Queries = 500
		}

		worlds := make([]World, numOf.Queries)
		for i := 0; i < numOf.Queries; i++ {
			worlds[i] = getWorld(db)
		}
		ctx.JSON(200, &worlds)
	})
	// MULTIPLE UPDATES
	h.GET("/updates", func(c context.Context, ctx *app.RequestContext) {
		var numOf NumOf

		if ctx.Bind(&numOf) != nil { // manage missing query num
			numOf.Queries = 1
		} else if numOf.Queries < 1 { // set at least 1
			numOf.Queries = 1
		} else if numOf.Queries > 500 { // set no more than 500
			numOf.Queries = 500
		}

		worlds := make([]World, numOf.Queries, numOf.Queries) // prealloc
		var err error = nil

		for i := 0; i < numOf.Queries; i++ {
			worlds[i], err = processWorld(db)

			if err != nil {
				fmt.Println(err)
				ctx.JSON(500, utils.H{"error": err})
				break
			}
		}

		ctx.JSON(200, worlds)
	})

	/* START SERVICE */

	h.Spin() // listen and serve on 0.0.0.0:8080
}