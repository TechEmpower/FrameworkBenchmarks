package providers

import (
	"github.com/goravel/framework/contracts/foundation"

	"goravel/routes"
)

type RouteServiceProvider struct{}

func (receiver *RouteServiceProvider) Register(app foundation.Application) {
}

func (receiver *RouteServiceProvider) Boot(app foundation.Application) {
	routes.Web()
}
