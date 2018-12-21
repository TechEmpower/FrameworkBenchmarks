package main

import (
	"fmt"
	"strconv"
	"math/rand"

	"github.com/kataras/iris"

	"database/sql"
	_ "github.com/lib/pq"

	"github.com/kataras/iris/context"
	"github.com/kataras/iris/middleware/logger"
	"github.com/kataras/iris/middleware/recover"
)

func main() {
	app := iris.New()
	app.Logger().SetLevel("debug")
	// Optionally, add two built'n handlers
	// that can recover from any http-relative panics
	// and log the requests to the terminal.
	app.Use(recover.New())
	app.Use(logger.New())

	connStr := "user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world host=tfb-database sslmode=disable"

	db, err := sql.Open("postgres", connStr)
	if err != nil {
		panic(err)
	}

	defer db.Close()

	err = db.Ping()
	if err != nil {
		panic(err)
	}

	fmt.Println("Successfully connected to PG!")

	// Helper methods

	type fetchingFunction func() map[string] int

	var getOneRandomWorld fetchingFunction = func() map[string]int {
		results := make(map[string]int)

		randInt := rand.Intn(10000) + 1
		sqlStatement := `SELECT id, randomnumber FROM world WHERE id=$1`

		rows, err := db.Query(sqlStatement, randInt)
		if err != nil {
			panic(err)
		}

		defer rows.Close()
		for rows.Next() {
			var id int
			var randomnumber int
			err = rows.Scan(&id, &randomnumber)
			if err != nil {
				panic(err)
			}
			results["id"] = id
			results["randomnumber"] = randomnumber
		}

		err = rows.Err()
		if err != nil {
			panic(err)
		}

		return results
	}

	var updateOneRandomWorld fetchingFunction = func() map[string]int {
		results := make(map[string]int)

		randInt1 := rand.Intn(10000) + 1
		randInt2 := rand.Intn(10000) + 1

		sqlStatement := `UPDATE world SET randomnumber=$1 WHERE id=$2 RETURNING id, randomnumber`

		rows, err := db.Query(sqlStatement, randInt1, randInt2)
		if err != nil {
			panic(err)
		}

		defer rows.Close()
		for rows.Next() {
			var id int
			var randomnumber int
			err = rows.Scan(&id, &randomnumber)
			if err != nil {
				panic(err)
			}
			results["id"] = id
			results["randomnumber"] = randomnumber
		}

		err = rows.Err()
		if err != nil {
			panic(err)
		}

		return results
	}

	// Routes
	app.Handle("GET", "/json", func(ctx context.Context) {
		ctx.Header("Server", "Iris")
		ctx.JSON(context.Map{"message": "Hello, World!"})
	})

	app.Handle("GET", "/plaintext", func(ctx context.Context) {
		ctx.Header("Server", "Iris")
		ctx.Text("Hello, World!")
	})

	app.Handle("GET", "/db", func(ctx context.Context) {

		results := getOneRandomWorld()

		ctx.Header("Server", "Iris")
		ctx.JSON(results)
	})

	app.Handle("GET", "/queries", func(ctx context.Context) {
		queryVal := ctx.FormValue("queries")
		q, err := strconv.Atoi(queryVal)
		if err != nil {
			q = 1
		}
		if q < 1 {
			q = 1
		} else if q > 500 {
			q = 500
		}
		
		results := make([]map[string]int, q)

		i := 0
		for i < q {
			results[i] = getOneRandomWorld()
			i = i + 1
		}
		ctx.Header("Server", "Iris")
		ctx.JSON(results)
	})

	app.Handle("GET", "/update", func(ctx context.Context) {
		queryVal := ctx.FormValue("queries")
		q, err := strconv.Atoi(queryVal)
		if err != nil {
			q = 1
		}
		if q < 1 {
			q = 1
		} else if q > 500 {
			q = 500
		}
		
		results := make([]map[string]int, q)

		i := 0
		for i < q {
			results[i] = updateOneRandomWorld()
			i = i + 1
		}
		ctx.Header("Server", "Iris")
		ctx.JSON(results)
	})

	app.Run(iris.Addr(":8080"), iris.WithoutServerError(iris.ErrServerClosed))
}
