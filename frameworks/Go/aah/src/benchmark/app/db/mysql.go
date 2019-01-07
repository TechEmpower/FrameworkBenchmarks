package db

import (
	"database/sql"
	"runtime"

	"aahframe.work"

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
	app := aah.App()
	if !app.IsEnvProfile("bm_mysql") {
		return
	}

	cfg := app.Config()
	var err error
	MySQL, err = sql.Open(
		cfg.StringDefault("datasource.benchmark.mysql.driver", "mysql"),
		cfg.StringDefault("datasource.benchmark.mysql.url", ""),
	)
	if err != nil {
		app.Log().Fatal(err)
	}

	if err = MySQL.Ping(); err != nil {
		app.Log().Fatal(err)
	}

	MySQL.SetMaxIdleConns(mysqlMaxConnCount)
	MySQL.SetMaxOpenConns(mysqlMaxConnCount)

	if MSworldSelectStmt, err = MySQL.Prepare("SELECT id, randomNumber FROM World WHERE id = ?"); err != nil {
		app.Log().Fatal(err)
	}
	if MSworldUpdateStmt, err = MySQL.Prepare("UPDATE World SET randomNumber = ? WHERE id = ?"); err != nil {
		app.Log().Fatal(err)
	}
	if MSfortuneSelectStmt, err = MySQL.Prepare("SELECT id, message FROM Fortune"); err != nil {
		app.Log().Fatal(err)
	}
}

// CloseMySQLDatabase initializes the Database.
func CloseMySQLDatabase(_ *aah.Event) {
	if MySQL != nil {
		_ = MySQL.Close()
	}
}
