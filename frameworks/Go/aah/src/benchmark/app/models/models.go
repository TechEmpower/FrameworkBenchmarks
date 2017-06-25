package models

import (
	"benchmark/app/db"
	"database/sql"
	"math/rand"
	"sort"

	"aahframework.org/aah.v0"
	"aahframework.org/log.v0"
)

var (
	// Prepare statements
	worldSelectStmt   *sql.Stmt
	worldUpdateStmt   *sql.Stmt
	fortuneSelectStmt *sql.Stmt

	worldRowCount = 10000
)

type (
	// Message is used for JSON reply.
	Message struct {
		Message string `json:"message"`
	}

	// World is used for database operation.
	World struct {
		ID           uint16 `json:"id"`
		RandomNumber uint16 `json:"randomNumber"`
	}

	// Fortune is used for database and reply.
	Fortune struct {
		ID      uint16 `json:"id"`
		Message string `json:"message"`
	}

	// Fortunes slice
	Fortunes []*Fortune

	// For sorting
	byMessage struct{ Fortunes }
)

//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
// Global methods
//___________________________________

// FetchRandomWorld method returns one world record info.
func FetchRandomWorld(w *World) error {
	err := worldSelectStmt.QueryRow(randomWorldNum()).Scan(&w.ID, &w.RandomNumber)
	return err
}

// GetRandomWorlds method returns world record from db randomly.
func GetRandomWorlds(count int) (*[]World, error) {
	worlds := make([]World, count)
	var err error
	for i := 0; i < count; i++ {
		w := &worlds[i]
		if err = FetchRandomWorld(w); err != nil {
			return nil, err
		}
	}
	return &worlds, nil
}

// UpdateRandomWorlds method updates random world records.
func UpdateRandomWorlds(count int) (*[]World, error) {
	worlds := make([]World, count)
	var err error
	for i := 0; i < count; i++ {
		w := &worlds[i]
		if err = FetchRandomWorld(w); err != nil {
			return nil, err
		}

		w.RandomNumber = uint16(randomWorldNum())
		if _, err = worldUpdateStmt.Exec(w.RandomNumber, w.ID); err != nil {
			return nil, err
		}
	}
	return &worlds, nil
}

// GetFortunes method returns fortunes records
func GetFortunes() (Fortunes, error) {
	rows, err := fortuneSelectStmt.Query()
	if err != nil {
		return nil, err
	}

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() {
		var fortune Fortune
		if err = rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			return nil, err
		}
		fortunes = append(fortunes, &fortune)
	}
	_ = rows.Close()

	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
	sort.Sort(byMessage{fortunes})
	return fortunes, nil
}

//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
// Unexported methods
//___________________________________

func randomWorldNum() int {
	return rand.Intn(worldRowCount) + 1
}

func prepareStatements(e *aah.Event) {
	var err error
	if worldSelectStmt, err = db.DB().Prepare("SELECT id, randomNumber FROM World WHERE id = ?"); err != nil {
		log.Fatal(err)
	}

	if worldUpdateStmt, err = db.DB().Prepare("UPDATE World SET randomNumber = ? WHERE id = ?"); err != nil {
		log.Fatal(err)
	}

	if fortuneSelectStmt, err = db.DB().Prepare("SELECT id, message FROM Fortune"); err != nil {
		log.Fatal(err)
	}
}

// Sorting interfaces
func (s Fortunes) Len() int            { return len(s) }
func (s Fortunes) Swap(i, j int)       { s[i], s[j] = s[j], s[i] }
func (s byMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }

func init() {
	aah.OnStart(prepareStatements, 2)
}
