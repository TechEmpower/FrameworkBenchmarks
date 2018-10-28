package models

import (
	"github.com/astaxie/beego/orm"
	_ "github.com/go-sql-driver/mysql"
)

const (
	// Database
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world"
	macIdleConnection  = 30
	maxConnectionCount = 256
)

func init() {
	orm.RegisterModel(new(World))
	orm.RegisterDataBase("default", "mysql", connectionString, macIdleConnection, maxConnectionCount)
}
