package config

import (
	"github.com/gin-gonic/gin/render"
	"github.com/goravel/framework/contracts/route"
	"github.com/goravel/framework/facades"
	"github.com/goravel/gin"
	ginfacades "github.com/goravel/gin/facades"
)

func init() {
	config := facades.Config()
	config.Add("http", map[string]any{
		"default": "gin",
		"drivers": map[string]any{
			"gin": map[string]any{
				"body_limit":   4096,
				"header_limit": 4096,
				"route": func() (route.Route, error) {
					return ginfacades.Route("gin"), nil
				},
				"template": func() (render.HTMLRender, error) {
					return gin.DefaultTemplate()
				},
			},
		},
		"host": "",
		"port": "8080",
	})
}
