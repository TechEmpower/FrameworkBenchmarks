package controllers

import (
	"strconv"

	"benchmark/app/models"

	"aahframe.work"
)

const helloWorldString = "Hello, World!"

// AppController struct application controller
type AppController struct {
	*aah.Context
}

// Plaintext method is for `/plaintext` test.
func (c *AppController) Plaintext() {
	c.Reply().Text(helloWorldString)
}

// JSON method is for `/json` test.
func (c *AppController) JSON() {
	c.Reply().JSON(models.Message{Message: helloWorldString})
}

//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
// MySQL DB based implementation
//___________________________________________________________________________

// World returns one world record randomly for `/db` test.
func (c *AppController) World() {
	world := new(models.World)
	if err := models.MySQLFetchRandomWorld(world); err != nil {
		c.Reply().InternalServerError().Text(err.Error())
		return
	}
	c.Reply().JSON(world)
}

// Worlds returns one world record randomly for `/db/queries` test.
func (c *AppController) Worlds() {
	c.handleResult(models.MySQLRandomWorlds(c.getCount()))
}

// UpdateWorlds updates record and returns those records for `/db/updates` test.
func (c *AppController) UpdateWorlds() {
	c.handleResult(models.MySQLUpdateRandomWorlds(c.getCount()))
}

// Fortunes method is for `/db/fortunes` test.
func (c *AppController) Fortunes() {
	fortunes, err := models.MySQLFortunes()
	if err != nil {
		c.Reply().InternalServerError().Text(err.Error())
		return
	}

	c.Reply().HTML(aah.Data{
		"Fortunes": fortunes,
	})
}

//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
// PostgreSQL DB based implementation
//___________________________________________________________________________

// PGWorld returns one world record randomly for `/pg-db` test.
func (c *AppController) PGWorld() {
	world := new(models.World)
	if err := models.PGFetchRandomWorld(world); err != nil {
		c.Reply().InternalServerError().Text(err.Error())
		return
	}
	c.Reply().JSON(world)
}

// PGWorlds returns one world record randomly for `/pg-db/queries` test.
func (c *AppController) PGWorlds() {
	c.handleResult(models.PGRandomWorlds(c.getCount()))
}

// PGUpdateWorlds updates record and returns those records for `/pg-db/updates` test.
func (c *AppController) PGUpdateWorlds() {
	c.handleResult(models.PGUpdateRandomWorlds(c.getCount()))
}

// PGFortunes method is for `/pg-db/fortunes` test.
func (c *AppController) PGFortunes() {
	fortunes, err := models.PGFortunes()
	if err != nil {
		c.Reply().InternalServerError().Text(err.Error())
		return
	}

	c.Reply().HTMLf("fortunes.html", aah.Data{
		"Fortunes": fortunes,
	})
}

//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
// Unexported methods
//___________________________________________________________________________

func (c *AppController) getCount() int {
	cnt, err := strconv.Atoi(c.Req.QueryValue("count"))
	if err != nil || cnt < 1 {
		cnt = 1
	} else if cnt > 500 {
		cnt = 500
	}
	return cnt
}

func (c *AppController) handleResult(worlds []models.World, err error) {
	if err != nil {
		c.Reply().InternalServerError().Text(err.Error())
		return
	}
	c.Reply().JSON(worlds)
}
