package config

import (
	"github.com/goravel/framework/facades"
)

func init() {
	config := facades.Config()
	config.Add("database", map[string]any{
		"default": "postgresql",
		"connections": map[string]any{
			"postgresql": map[string]any{
				"driver":   "postgresql",
				"host":     config.Env("DB_HOST", "tfb-database"),
				"port":     config.Env("DB_PORT", 5432),
				"database": config.Env("DB_DATABASE", "hello_world"),
				"username": config.Env("DB_USERNAME", "benchmarkdbuser"),
				"password": config.Env("DB_PASSWORD", "benchmarkdbpass"),
				"sslmode":  "disable",
				"timezone": "UTC",
				"prefix":   "",
				"singular": true,
			},
		},
		"pool": map[string]any{
			"max_idle_conns":    100,
			"max_open_conns":    2000,
			"conn_max_idletime": 3600,
			"conn_max_lifetime": 3600,
		},
	})
}
