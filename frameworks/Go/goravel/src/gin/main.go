package main

import (
	"github.com/goravel/framework/facades"
	"github.com/goravel/framework/foundation"

	"goravel/config"
)

func main() {
	app := foundation.NewApplication()

	// Bootstrap the application
	app.Boot()

	// Bootstrap the config.
	config.Boot()

	// Start HTTP server by facades.Route().
	go func() {
		if err := facades.Route().Run(); err != nil {
			facades.Log().Errorf("Route run error: %v", err)
		}
	}()

	select {}
}
