package main

import (
	"log"
	"math/rand"
	"github.com/astaxie/beego"
	"github.com/astaxie/beego/orm"

	_ "github.com/go-sql-driver/mysql"
	//"runtime"
)

const (
	// Database
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world"
	worldRowCount      = 10000
	macIdleConnection  = 30
	maxConnectionCount = 256

	helloWorldString = "Hello, World!"
)

var (
	helloWorldBytes = []byte(helloWorldString)
)

type MessageStruct struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `orm:"pk" json:"id"`
	RandomNumber uint16 `orm:"column(randomNumber)" json:"randomNumber"`
}

type JsonController struct {
	beego.Controller
}

func (this *JsonController) Get() {
	m := MessageStruct{"Hello, World!"}
	this.Data["json"] = &m
	this.ServeJson()
}

type PlaintextController struct {
	beego.Controller
}

func (this *PlaintextController) Get() {
	this.Ctx.Output.Header("Content-Type", "text/plain")
	this.Ctx.Output.Body(helloWorldBytes)
}

type DBController struct {
	beego.Controller
}

func (this *DBController) Get() {
	o := orm.NewOrm()
	w := World{Id: uint16(rand.Intn(worldRowCount) + 1)}
	err := o.Read(&w)
	if err != nil {
		log.Fatalf("Error read world row: %s", err.Error())
	}
	this.Data["json"] = &w
	this.ServeJson()
}

func main() {
	//don't need this set, beego default set it
	//runtime.GOMAXPROCS(runtime.NumCPU())
	beego.RunMode = "prod"
	beego.Router("/json", &JsonController{})
	beego.Router("/db", &DBController{})
	beego.Router("/plaintext", &PlaintextController{})
	beego.Run()
}

func init() {
	orm.RegisterModel(new(World))
	orm.RegisterDataBase("default", "mysql", connectionString, macIdleConnection, maxConnectionCount)
}
