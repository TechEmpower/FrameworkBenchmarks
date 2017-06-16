package db

import (
	"database/sql"
	"fmt"
	"os"
	"runtime"

	"aahframework.org/aah.v0"
	"aahframework.org/log.v0"

	// mysql driver
	_ "github.com/go-sql-driver/mysql"
)

var (
	maxConnsCount = runtime.NumCPU() * 2
	db            *sql.DB
)

// DB instance
func DB() *sql.DB {
	return db
}

// DatabaseInit initializes the Database.
func DatabaseInit(e *aah.Event) {
	cfg := aah.AppConfig()
	dbHost := os.Getenv("DBHOST")
	if dbHost == "" {
		dbHost = "localhost"
	}

	dbURL := fmt.Sprintf(cfg.StringDefault("datasource.benchmark.url", ""), dbHost)

	var err error
	db, err = sql.Open(cfg.StringDefault("datasource.benchmark.driver", ""), dbURL)
	if err != nil {
		log.Fatal(err)
	}

	if err = db.Ping(); err != nil {
		log.Fatal(err)
	}

	db.SetMaxIdleConns(maxConnsCount)
	db.SetMaxOpenConns(maxConnsCount)
}

// DatabaseClose initializes the Database.
func DatabaseClose(e *aah.Event) {
	if db != nil {
		_ = db.Close()
	}
}

func init() {
	aah.OnStart(DatabaseInit)
	aah.OnShutdown(DatabaseClose)
}
