package db

import (
	"github.com/coocood/qbs"
	"github.com/revel/revel"
)

func InitQbs(maxConn int) {
	var (
		found        bool
		driver, spec string
	)
	if driver, found = revel.Config.String("db.driver"); !found {
		revel.ERROR.Fatal("No db.driver found.")
	}
	if spec, found = revel.Config.String("db.spec"); !found {
		revel.ERROR.Fatal("No db.spec found.")
	}

	// QBS uses snake case by default; override the name convention.
	qbs.ColumnNameToFieldName = noConvert
	qbs.FieldNameToColumnName = noConvert
	qbs.TableNameToStructName = noConvert
	qbs.StructNameToTableName = noConvert

	qbs.Register(driver, spec, "", qbs.NewMysql())
	qbs.ChangePoolSize(maxConn)
}

func noConvert(s string) string { return s }
