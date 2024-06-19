package config

import (
	"github.com/goravel/framework/facades"
)

func init() {
	config := facades.Config()
	config.Add("cache", map[string]any{
		"default": "memory",
		"stores": map[string]any{
			"memory": map[string]any{
				"driver": "memory",
			},
		},
		"prefix": "goravel_cache",
	})
}
