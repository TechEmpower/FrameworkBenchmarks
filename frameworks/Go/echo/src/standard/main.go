package main

import (
	"common"

	"github.com/labstack/echo"
	"github.com/labstack/echo/engine/standard"
)

func main() {
	e := echo.New()
	e.SetRenderer(common.Template)
	common.InitRoutes(e)
	common.InitPostgres()
	e.Run(standard.New(":8080"))
}
