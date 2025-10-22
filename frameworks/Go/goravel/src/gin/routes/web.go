package routes

import (
	"github.com/goravel/framework/facades"

	"goravel/app/http/controllers"
)

func Web() {
	testController := controllers.NewTestController()
	facades.Route().Get("/plaintext", testController.Plaintext)
	facades.Route().Get("/json", testController.JSON)
	facades.Route().Get("/db", testController.DB)
	facades.Route().Get("/queries", testController.Queries)
	facades.Route().Get("/update", testController.Update)
	facades.Route().Get("/fortunes", testController.Fortunes)
	facades.Route().Get("/cached-worlds", testController.CacheQueries)

}
