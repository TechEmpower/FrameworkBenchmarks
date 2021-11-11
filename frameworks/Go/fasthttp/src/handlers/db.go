package handlers

import (
	"context"
	"fmt"

	"github.com/jackc/pgx/v4/pgxpool"
)

const (
	dbHost  = "tfb-database"
	dbPort  = 5432
	dbUser  = "benchmarkdbuser"
	dbPaswd = "benchmarkdbpass"
	dbName  = "hello_world"

	worldSelectSQL      = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldSelectCacheSQL = "SELECT id, randomNumber FROM World LIMIT $1"
	worldUpdateSQL      = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneSelectSQL    = "SELECT id, message FROM Fortune"
)

var db *pgxpool.Pool

func InitDB(maxConn int) error {
	pgx, err := newPGX(maxConn)
	if err != nil {
		return err
	}

	db = pgx

	return nil
}

func CloseDB() {
	db.Close()
}

func newPGX(maxConn int) (*pgxpool.Pool, error) {
	dsn := fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s pool_max_conns=%d",
		dbHost, dbPort, dbUser, dbPaswd, dbName, maxConn,
	)

	return pgxpool.Connect(context.Background(), dsn)
}
