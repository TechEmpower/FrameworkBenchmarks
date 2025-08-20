package main

import (
	"context"
	"math/rand/v2"
	"slices"
	"strconv"
	"time"

	"github.com/indigo-web/indigo/http"
	"github.com/indigo-web/indigo/http/mime"

	"indigo/app/models"
	"indigo/app/templates"
)

type (
	App struct {
		DB    *DB
		Cache models.Worlds
	}
)

func NewApp(DB *DB) *App {
	return &App{DB: DB}
}

func (app *App) PopulateCache(ctx context.Context) error {
	cache := make(models.Worlds, 10000)

	err := app.DB.FillWorlds(context.Background(), cache)
	if err != nil {
		return err
	}

	app.Cache = cache

	return nil
}

func (app *App) HandleJSON(request *http.Request) *http.Response {
	return request.Respond().Header("Date", time.Now().Format(time.RFC1123)).Header("Server", "indigo").JSON(&models.Message{
		Message: "Hello, World!",
	})
}

func (app *App) HandleDB(request *http.Request) *http.Response {
	world := &models.World{
		ID: rand.IntN(10000) + 1,
	}

	err := app.DB.FillWorldByID(context.Background(), world)
	if err != nil {
		return http.Error(request, err)
	}

	return request.Respond().Header("Date", time.Now().Format(time.RFC1123)).Header("Server", "indigo").JSON(world)
}

func (app *App) HandleQuery(request *http.Request) *http.Response {
	n := normalizeNumber(request.Params.Lookup("n"))

	i, worlds := 0, make(models.Worlds, n)

	for i = range worlds {
		worlds[i].ID = rand.IntN(10000) + 1
	}

	err := app.DB.FillWorldsByID(context.Background(), worlds)
	if err != nil {
		return http.Error(request, err)
	}

	return request.Respond().Header("Date", time.Now().Format(time.RFC1123)).Header("Server", "indigo").JSON(&worlds)
}

func (app *App) HandleUpdate(request *http.Request) *http.Response {
	n := normalizeNumber(request.Params.Lookup("n"))

	i, worlds := 0, make(models.Worlds, n)

	for i = range worlds {
		worlds[i].ID = rand.IntN(10000) + 1
	}

	err := app.DB.FillWorldsByID(context.Background(), worlds)
	if err != nil {
		return http.Error(request, err)
	}

	for i = range worlds {
		worlds[i].RandomNumber = rand.IntN(10000) + 1
	}

	slices.SortFunc(worlds, func(a, b models.World) int {
		return a.ID - b.ID
	})

	err = app.DB.UpdateWorlds(context.Background(), worlds)
	if err != nil {
		return http.Error(request, err)
	}

	return request.Respond().Header("Date", time.Now().Format(time.RFC1123)).Header("Server", "indigo").JSON(&worlds)
}

func (app *App) HandleCachedQuery(request *http.Request) *http.Response {
	n := normalizeNumber(request.Params.Lookup("n"))

	i, worlds := 0, make(models.Worlds, n)

	for i = range worlds {
		worlds[i] = app.Cache[rand.Int32N(10000)]
	}

	return request.Respond().Header("Date", time.Now().Format(time.RFC1123)).Header("Server", "indigo").JSON(&worlds)
}

func (app *App) HandleFortune(request *http.Request) *http.Response {
	fortunes, err := app.DB.GetFortunes(context.Background())
	if err != nil {
		return http.Error(request, err)
	}

	fortunes = append(fortunes, models.Fortune{
		Message: "Additional fortune added at request time.",
	})

	slices.SortFunc(fortunes, func(a, b models.Fortune) int {
		if a.Message < b.Message {
			return -1
		} else if a.Message > b.Message {
			return 1
		}

		return 0
	})

	return request.Respond().Header("Date", time.Now().Format(time.RFC1123)).Header("Server", "indigo").ContentType(mime.HTML + "; charset=UTF-8").String(templates.HTMLFortunes(fortunes))
}

func (app *App) HandlePlaintext(request *http.Request) *http.Response {
	return request.Respond().Header("Date", time.Now().Format(time.RFC1123)).Header("Server", "indigo").ContentType(mime.Plain).String("Hello, World!")
}

func normalizeNumber(nString string, found bool) int {
	if !found {
		nString = "0"
	}

	n, err := strconv.Atoi(nString)
	if err != nil {
		n = 0
	}

	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	return n
}
