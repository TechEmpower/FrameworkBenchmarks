// aah application initialization - configuration, server extensions, middleware's, etc.
// Customize it per application use case.

package main

import (
	"benchmark/app/db"

	"aahframe.work"
)

func init() {
	app := aah.App()

	app.OnStart(db.InitMySQLDatabase)
	app.OnStart(db.InitPostgreSQLDatabase)
	app.OnStart(func(_ *aah.Event) {
		app.SecurityManager().AntiCSRF.Enabled = false
	})

	app.OnPostShutdown(db.CloseMySQLDatabase)
	app.OnPostShutdown(db.ClosePostgreSQLDatabase)

	app.HTTPEngine().Middlewares(
		aah.RouteMiddleware,
		aah.ActionMiddleware,
	)
}
