package db

import (
	"github.com/coocood/qbs"
	"github.com/robfig/revel"
)

var (
	Driver string
	Spec   string
)

func Init() {
	var found bool
	if Driver, found = revel.Config.String("db.driver"); !found {
		revel.ERROR.Fatal("No db.driver found.")
	}
	if Spec, found = revel.Config.String("db.spec"); !found {
		revel.ERROR.Fatal("No db.spec found.")
	}

	// QBS uses snake case by default; override the name convention.
	qbs.ColumnNameToFieldName = noConvert
	qbs.FieldNameToColumnName = noConvert
	qbs.TableNameToStructName = noConvert
	qbs.StructNameToTableName = noConvert

	qbs.Register(Driver, Spec, "", qbs.NewMysql())
}

func noConvert(s string) string { return s }
