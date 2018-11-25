package db

import (
	"database/sql"
	"runtime"

	"aahframework.org/aah.v0"
	"aahframework.org/log.v0"

	// mysql driver
	_ "github.com/go-sql-driver/mysql"
)

// MySQL database connection and prepared statements
var (
	MySQL               *sql.DB
	MSworldSelectStmt   *sql.Stmt
	MSworldUpdateStmt   *sql.Stmt
	MSfortuneSelectStmt *sql.Stmt

	mysqlMaxConnCount = runtime.NumCPU() * 2
)

// InitMySQLDatabase initializes the Database.
func InitMySQLDatabase(_ *aah.Event) {
	cfg := aah.AppConfig()
	if aah.AppProfile() != "bm_mysql" {
		return
	}

	var err error
	MySQL, err = sql.Open(
		cfg.StringDefault("datasource.benchmark.mysql.driver", "mysql"),
		cfg.StringDefault("datasource.benchmark.mysql.url", ""),
	)
	if err != nil {
		log.Fatal(err)
	}

	if err = MySQL.Ping(); err != nil {
		log.Fatal(err)
	}

	MySQL.SetMaxIdleConns(mysqlMaxConnCount)
	MySQL.SetMaxOpenConns(mysqlMaxConnCount)

	if MSworldSelectStmt, err = MySQL.Prepare("SELECT id, randomNumber FROM World WHERE id = ?"); err != nil {
		log.Fatal(err)
	}
	if MSworldUpdateStmt, err = MySQL.Prepare("UPDATE World SET randomNumber = ? WHERE id = ?"); err != nil {
		log.Fatal(err)
	}
	if MSfortuneSelectStmt, err = MySQL.Prepare("SELECT id, message FROM Fortune"); err != nil {
		log.Fatal(err)
	}
}

// CloseMySQLDatabase initializes the Database.
func CloseMySQLDatabase(_ *aah.Event) {
	if MySQL != nil {
		_ = MySQL.Close()
	}
}
