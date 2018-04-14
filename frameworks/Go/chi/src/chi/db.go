package main

import (
	"flag"
	"log"

	"github.com/jackc/pgx"
)

var (
	defaultDB *pgx.ConnPool
)

func initDBConnection(pgURL string) {
	config, err := pgx.ParseConnectionString(pgURL)
	if err != nil {
		flag.PrintDefaults()
		log.Fatalf("Error parsing db URL: %v", err)
	}

	connPoolConfig := pgx.ConnPoolConfig{
		ConnConfig:     config,
		MaxConnections: maxConnectionCount,
	}

	defaultDB, err = pgx.NewConnPool(connPoolConfig)
	if err != nil {
		flag.PrintDefaults()
		log.Fatalf("Error opening database: %v", err)
	}

	_, err = defaultDB.Prepare("worldSelect", worldSelect)
	if err != nil {
		flag.PrintDefaults()
		log.Fatal(err)
	}
	_, err = defaultDB.Prepare("fortuneSelect", fortuneSelect)
	if err != nil {
		flag.PrintDefaults()
		log.Fatal(err)
	}
	_, err = defaultDB.Prepare("worldUpdate", worldUpdate)
	if err != nil {
		flag.PrintDefaults()
		log.Fatal(err)
	}
}
