package main

import (
  "database/sql"
  "encoding/json"
  "log"
  "net/http"
  "time"

  "github.com/go-martini/martini"
  _ "github.com/lib/pq"
)

var (
	db                    *sql.DB
	worldSelectPrepared   *sql.Stmt
	worldUpdatePrepared   *sql.Stmt
	fortuneSelectPrepared *sql.Stmt
)

type World struct {
  Id           uint16 `json:"id"`
  RandomNumber uint16 `json:"randomNumber"`
}


func main() {
  initDB()

  m := martini.Classic()

  m.Use(setRequiredHeaders)

  m.Group("/bench", func (r martini.Router) {
    r.Get("/plaintext", plaintextHandler)
    r.Get("/json"     , jsonHandler)
    r.Get("/single"   , singleQueryHandler)
    r.Get("/multiple" , multipleQueriesHandler)
    r.Get("/update"   , updatesHandler)
  })

  m.RunOnAddr(":8080")
}

// Initialize the connection to the database
func initDB() {
  driverName := "postgres"
  connectionString := "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world?sslmode=disable"
  worldSelect := "SELECT id, randomNumber FROM World WHERE id = $1"
	worldUpdate := "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneSelect := "SELECT id, message FROM Fortune"
	// worldRowCount := 10000
	maxConnections := 256

  db, err := sql.Open(driverName, connectionString)

  if err != nil {
    log.Fatalf("Error opening database: %v", err)
  }

  db.SetMaxIdleConns(maxConnections)
  db.SetMaxOpenConns(maxConnections)

  worldSelectPrepared, err = db.Prepare(worldSelect)
	if err != nil {
		log.Fatal(err)
	}
	worldUpdatePrepared, err = db.Prepare(worldUpdate)
	if err != nil {
		log.Fatal(err)
	}
	fortuneSelectPrepared, err = db.Prepare(fortuneSelect)
	if err != nil {
		log.Fatal(err)
	}
}

// Create middleware to add Server and Date headers to all
// routes.
func setRequiredHeaders(w http.ResponseWriter, r *http.Request) {
  t := time.Now();

  w.Header().Set("Server", "Martini")
  w.Header().Set("Date", t.Format(time.RFC1123))
}

// Route handlers
func plaintextHandler() string {
  return "Hello, World!"
}

func jsonHandler(w http.ResponseWriter) []byte {
  message := map[string]interface{}{"message": "Hello, World!"}
  w.Header().Set("Content-Type", "application/json")

  res, _ := json.Marshal(message);
  return res
}

func singleQueryHandler(w http.ResponseWriter) {
  var world World

  w.Header().Set("Content-Type", "application/json")

  err := worldSelectPrepared.QueryRow(RandomNumber()).Scan(&world.Id, &world.RandomNumber)
  if err != nil {
    log.Fatalf("Error scanning world row: %s", err.Error())
  }

  // No need to return anything as we are writing straight
  // to the response itself
  json.NewEncoder(w).Encode(&world)
}

func multipleQueriesHandler(w http.ResponseWriter, r *http.Request) {
  queries := SanitizeQueries(r)
  worlds := make([]World, queries)

  w.Header().Set("Content-Type", "application/json")

  for i := 0; i < queries; i += 1 {
    err := worldSelectPrepared.QueryRow(RandomNumber()).Scan(&worlds[i].Id, &worlds[i].RandomNumber)

    if err != nil {
      log.Fatalf("Error scanning world row: %v", err)
    }
  }

  json.NewEncoder(w).Encode(worlds)
}

func updatesHandler(w http.ResponseWriter, r *http.Request) {
	queries := SanitizeQueries(r)

	worlds := make([]World, queries)
	for i := 0; i < queries; i++ {
		if err := worldSelectPrepared.QueryRow(RandomNumber()).Scan(&worlds[i].Id, &worlds[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %v", err)
		}
		worlds[i].RandomNumber = uint16(RandomNumber())
		if _, err := worldUpdatePrepared.Exec(worlds[i].RandomNumber, worlds[i].Id); err != nil {
			log.Fatalf("Error updating world row: %v", err)
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(worlds)
}
