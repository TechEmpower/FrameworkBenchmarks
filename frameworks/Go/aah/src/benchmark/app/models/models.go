package models

import (
	"fmt"
	"math/rand"
	"sort"

	"benchmark/app/db"
)

var worldRowCount = 10000

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
)

//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
// MySQL DB based implementation
//___________________________________________________________________________

// MySQLFetchRandomWorld method returns one world record info.
func MySQLFetchRandomWorld(w *World) error {
	err := db.MSworldSelectStmt.QueryRow(randomWorldNum()).Scan(&w.ID, &w.RandomNumber)
	return err
}

// MySQLRandomWorlds method returns world record from db randomly.
func MySQLRandomWorlds(count int) ([]World, error) {
	worlds := make([]World, count)
	var err error
	for i := 0; i < count; i++ {
		w := &worlds[i]
		if err = MySQLFetchRandomWorld(w); err != nil {
			return nil, err
		}
	}
	return worlds, nil
}

// MySQLUpdateRandomWorlds method updates random world records.
func MySQLUpdateRandomWorlds(count int) ([]World, error) {
	worlds := make([]World, count)
	var err error
	for i := 0; i < count; i++ {
		w := &worlds[i]
		if err = MySQLFetchRandomWorld(w); err != nil {
			return nil, err
		}

		w.RandomNumber = uint16(randomWorldNum())
		if _, err = db.MSworldUpdateStmt.Exec(w.RandomNumber, w.ID); err != nil {
			return nil, err
		}
	}
	return worlds, nil
}

// MySQLFortunes method returns fortunes records
func MySQLFortunes() (Fortunes, error) {
	rows, err := db.MSfortuneSelectStmt.Query()
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
	sortFortunesByMessage(fortunes)
	return fortunes, nil
}

//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
// PostgreSQL DB based implementation
//___________________________________________________________________________

// PGFetchRandomWorld method returns one world record info.
func PGFetchRandomWorld(w *World) error {
	return db.PostgreSQL.QueryRow("worldSelectStmt", randomWorldNum()).Scan(&w.ID, &w.RandomNumber)
}

// PGRandomWorlds method returns world record from db randomly.
func PGRandomWorlds(count int) ([]World, error) {
	worlds := make([]World, count)
	var err error
	for i := 0; i < count; i++ {
		w := &worlds[i]
		if err = PGFetchRandomWorld(w); err != nil {
			return nil, err
		}
	}
	return worlds, nil
}

// PGUpdateRandomWorlds method updates random world records.
func PGUpdateRandomWorlds(count int) ([]World, error) {
	worlds := make([]World, count)
	var err error
	for i := 0; i < count; i++ {
		w := &worlds[i]
		if err = PGFetchRandomWorld(w); err != nil {
			return nil, err
		}
		w.RandomNumber = uint16(randomWorldNum())
	}

	// sorting is required for insert deadlock prevention.
	sortWorldsByID(worlds)

	txn, err := db.PostgreSQL.Begin()
	if err != nil {
		return nil, fmt.Errorf("Error starting transaction: %s", err)
	}
	for i := 0; i < count; i++ {
		w := &worlds[i]
		if _, err = txn.Exec("worldUpdateStmt", w.RandomNumber, w.ID); err != nil {
			return nil, fmt.Errorf("Error updating world row %d: %s", i, err)
		}
	}
	if err = txn.Commit(); err != nil {
		return nil, fmt.Errorf("Error when commiting world rows: %s", err)
	}

	return worlds, nil
}

// PGFortunes method returns fortunes records
func PGFortunes() (Fortunes, error) {
	rows, err := db.PostgreSQL.Query("fortuneSelectStmt")
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
	rows.Close()

	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
	sortFortunesByMessage(fortunes)
	return fortunes, nil
}

//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
// Unexported methods
//___________________________________________________________________________

func randomWorldNum() int {
	return rand.Intn(worldRowCount) + 1
}

func sortFortunesByMessage(fortunes Fortunes) {
	sort.Slice(fortunes, func(i, j int) bool { return fortunes[i].Message < fortunes[j].Message })
}

func sortWorldsByID(worlds []World) {
	sort.Slice(worlds, func(i, j int) bool { return worlds[i].ID < worlds[j].ID })
}
