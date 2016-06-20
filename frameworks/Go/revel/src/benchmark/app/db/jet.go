package db

import (
	"github.com/eaigner/jet"
	"github.com/revel/revel"
)

var (
	Jet *jet.Db
)

func InitJet() {
	// Read configuration.
	var found bool
	var driver, spec string
	if driver, found = revel.Config.String("db.driver"); !found {
		revel.ERROR.Fatal("No db.driver found.")
	}
	if spec, found = revel.Config.String("db.spec"); !found {
		revel.ERROR.Fatal("No db.spec found.")
	}

	// Open a connection.
	var err error
	Jet, err = jet.Open(driver, spec)
	if err != nil {
		revel.ERROR.Fatal(err)
	}
	// Jet.SetLogger(jet.NewLogger(os.Stdout))
}
