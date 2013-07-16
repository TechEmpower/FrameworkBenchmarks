package db

import (
	"github.com/eaigner/jet"
	"github.com/robfig/revel"
	// "os"
)

var (
	Jet    jet.Db
	Driver string
	Spec   string
)

func Init() {
	// Read configuration.
	var found bool
	if Driver, found = revel.Config.String("db.driver"); !found {
		revel.ERROR.Fatal("No db.driver found.")
	}
	if Spec, found = revel.Config.String("db.spec"); !found {
		revel.ERROR.Fatal("No db.spec found.")
	}

	// Open a connection.
	var err error
	Jet, err = jet.Open(Driver, Spec)
	if err != nil {
		revel.ERROR.Fatal(err)
	}
	// Jet.SetLogger(jet.NewLogger(os.Stdout))
}
