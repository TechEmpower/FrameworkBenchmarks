package controllers

import (
	"strconv"
	"strings"
	"sync"

	"benchmark/app/models"

	"aahframework.org/aah.v0"
	"aahframework.org/ahttp.v0"
)

var (
	helloWorldMsg = "Hello, World!"
	messagePool   = sync.Pool{New: func() interface{} { return &models.Message{} }}
)

// AppController struct application controller
type AppController struct {
	*aah.Context
}

// Plaintext method is for `/plaintext` test.
func (a *AppController) Plaintext() {
	a.Reply().Text(helloWorldMsg)
}

// JSON method is for `/json` test.
func (a *AppController) JSON() {
	m := messagePool.Get().(*models.Message)
	m.Message = helloWorldMsg
	a.Reply().JSON(m)
}

// GetWorld returns one world record randomly for `/db` test.
func (a *AppController) GetWorld() {
	world := new(models.World)
	if err := models.FetchRandomWorld(world); err != nil {
		a.Reply().InternalServerError().Text(err.Error())
		return
	}
	a.Reply().JSON(world)
}

// GetWorlds returns one world record randomly for `/queries` test.
func (a *AppController) GetWorlds() {
	a.handleResult(models.GetRandomWorlds(a.getCount()))
}

// UpdateWorlds updates record and returns those records for `/updates` test.
func (a *AppController) UpdateWorlds() {
	a.handleResult(models.UpdateRandomWorlds(a.getCount()))
}

// Fortunes method is for `/fortunes` test.
func (a *AppController) Fortunes() {
	fortunes, err := models.GetFortunes()
	if err != nil {
		a.Reply().InternalServerError().Text(err.Error())
		return
	}

	a.Reply().HTML(aah.Data{
		"Fortunes": fortunes,
	})
}

// Finally interceptor
func (a *AppController) Finally() {
	// TFB requirements
	// Removing charset for json and plaintext
	if !strings.HasPrefix(a.Reply().ContType, ahttp.ContentTypeHTML.Mime) {
		a.Reply().ContType = a.Reply().ContType[:strings.IndexByte(a.Reply().ContType, ';')]
	}
}

func (a *AppController) getCount() int {
	cnt, err := strconv.Atoi(a.Req.QueryValue("count"))
	if err != nil || cnt < 1 {
		cnt = 1
	} else if cnt > 500 {
		cnt = 500
	}
	return cnt
}

func (a *AppController) handleResult(worlds *[]models.World, err error) {
	if err != nil {
		a.Reply().InternalServerError().Text(err.Error())
		return
	}
	a.Reply().JSON(worlds)
}
