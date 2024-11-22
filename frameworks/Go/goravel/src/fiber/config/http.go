package config

import (
	fiberfacades "github.com/goravel/fiber/facades"
	"github.com/goravel/framework/contracts/route"
	"github.com/goravel/framework/facades"
)

func init() {
	config := facades.Config()
	config.Add("http", map[string]any{
		"default": "fiber",
		"drivers": map[string]any{
			"fiber": map[string]any{
				"prefork":      true,
				"body_limit":   4096,
				"header_limit": 4096,
				"route": func() (route.Route, error) {
					return fiberfacades.Route("fiber"), nil
				},
			},
		},
		"host": "",
		"port": "8080",
	})
}
