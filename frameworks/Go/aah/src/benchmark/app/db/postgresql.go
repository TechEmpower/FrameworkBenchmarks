package db

import (
	"runtime"

	"aahframework.org/aah.v0"
	"aahframework.org/log.v0"
	"github.com/jackc/pgx"
)

// PostgreSQL database connection and statements
var (
	PostgreSQL          *pgx.ConnPool
	PGWorldSelectStmt   *pgx.PreparedStatement
	PGWorldUpdateStmt   *pgx.PreparedStatement
	PGFortuneSelectStmt *pgx.PreparedStatement

	postgresqlMaxConnCount = runtime.NumCPU() * 4
)

// InitPostgreSQLDatabase initializes the Database.
func InitPostgreSQLDatabase(_ *aah.Event) {
	cfg := aah.AppConfig()
	if aah.AppProfile() != "bm_postgresql" {
		return
	}

	config := pgx.ConnPoolConfig{
		ConnConfig: pgx.ConnConfig{
			Host:     cfg.StringDefault("datasource.benchmark.postgresql.host", ""),
			Port:     uint16(cfg.IntDefault("datasource.benchmark.postgresql.port", 5432)),
			User:     cfg.StringDefault("datasource.benchmark.postgresql.user", ""),
			Password: cfg.StringDefault("datasource.benchmark.postgresql.password", ""),
			Database: cfg.StringDefault("datasource.benchmark.postgresql.dbname", ""),
		},
		MaxConnections: postgresqlMaxConnCount,
	}

	config.AfterConnect = func(conn *pgx.Conn) error {
		var err error
		if PGWorldSelectStmt, err = conn.Prepare("worldSelectStmt", "SELECT id, randomNumber FROM World WHERE id = $1"); err != nil {
			log.Fatal(err)
		}
		if PGWorldUpdateStmt, err = conn.Prepare("worldUpdateStmt", "UPDATE World SET randomNumber = $1 WHERE id = $2"); err != nil {
			log.Fatal(err)
		}
		if PGFortuneSelectStmt, err = conn.Prepare("fortuneSelectStmt", "SELECT id, message FROM Fortune"); err != nil {
			log.Fatal(err)
		}
		return nil
	}

	var err error
	PostgreSQL, err = pgx.NewConnPool(config)
	if err != nil {
		log.Fatal(err)
	}
}

// ClosePostgreSQLDatabase initializes the Database.
func ClosePostgreSQLDatabase(_ *aah.Event) {
	if PostgreSQL != nil {
		PostgreSQL.Close()
	}
}
