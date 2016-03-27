package main

import (
	"common"

	"github.com/labstack/echo"
	"github.com/labstack/echo/engine/fasthttp"
)

func main() {
	e := echo.New()
	e.SetRenderer(common.Template)
	common.InitRoutes(e)
	common.InitPostgres()
	e.Run(fasthttp.New(":8080"))
}
