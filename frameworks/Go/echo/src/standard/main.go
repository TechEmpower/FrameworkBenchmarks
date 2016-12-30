package main

import (
	"common"

	"github.com/labstack/echo"
)

func main() {
	e := echo.New()
	e.Renderer = common.Template
	common.InitRoutes(e)
	common.InitPostgres()
	e.Start(":8080")
}
