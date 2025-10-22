package config

import (
	"github.com/goravel/framework/cache"
	"github.com/goravel/framework/console"
	"github.com/goravel/framework/contracts/foundation"
	"github.com/goravel/framework/database"
	"github.com/goravel/framework/facades"
	"github.com/goravel/framework/http"
	"github.com/goravel/framework/log"
	"github.com/goravel/framework/route"
	"github.com/goravel/framework/validation"
	"github.com/goravel/gin"

	"goravel/app/providers"
)

// Boot Start all init methods of the current folder to bootstrap all config.
func Boot() {}

func init() {
	config := facades.Config()
	config.Add("app", map[string]any{
		"name":  "Goravel",
		"env":   "production",
		"debug": false,
		"key":   config.Env("APP_KEY", ""),
		"providers": []foundation.ServiceProvider{
			&log.ServiceProvider{},
			&console.ServiceProvider{},
			&database.ServiceProvider{},
			&cache.ServiceProvider{},
			&http.ServiceProvider{},
			&route.ServiceProvider{},
			&validation.ServiceProvider{},
			&providers.RouteServiceProvider{},
			&gin.ServiceProvider{},
		},
	})
}
