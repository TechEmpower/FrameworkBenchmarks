import veb
import time
import rand
import db.pg

pub struct Context {
    veb.Context
}

pub struct App {
pub mut:
	db pg.DB
}

pub fn (app &App) plaintext(mut ctx Context) veb.Result {
	ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
	return ctx.text('Hello, World!')
}

pub fn (app &App) json(mut ctx Context) veb.Result {
	obj := {'message': 'Hello, World!'}
	ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
    return ctx.json(obj)
}

struct World {
	id           int @[primary; sql: serial]
mut:
	randomnumber int
}

pub fn (app &App) db(mut ctx Context) veb.Result {
	r := rand.int_in_range(1, 10000) or { return ctx.text('rand error') }
	mut world := sql app.db {
		select from World where id == r
	} or { return ctx.text('db error') }
	ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
	return ctx.json(world.first())
}

pub fn (app &App) queries(mut ctx Context) veb.Result {
	mut q := ctx.query['q'].int()
	if q < 1 { q = 1 } else if q > 500 { q = 500 }
	mut world := []World{}
	for _ in 0..q {
		r := rand.int_in_range(1, 10000) or { return ctx.text('rand error') }
		world << sql app.db {
			select from World where id == r
		} or { return ctx.text('db error') }
	}
	ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
	return ctx.json(world)
}

pub fn (app &App) update(mut ctx Context) veb.Result {
	mut q := ctx.query['q'].int()
	if q < 1 { q = 1 } else if q > 500 { q = 500 }
	mut world := []World{}
	for _ in 0..q {
		r := rand.int_in_range(1, 10000) or { return ctx.text('rand error') }
		world << sql app.db {
			select from World where id == r
		} or { return ctx.text('db error') }
		world.last().randomnumber = rand.int_in_range(1, 10000) or { return ctx.text('rand error') }
		sql app.db {
			update World set randomnumber = world.last().randomnumber where id == world.last().id
		} or { return ctx.text('db error') }
	}
	ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
	return ctx.json(world)
}

struct Fortune {
	id      int @[primary; sql: serial]
	message string
}

pub fn (app &App) fortunes(mut ctx Context) veb.Result {
	mut fortunes := sql app.db {
		select from Fortune
	} or { return ctx.text('db error') }
	fortunes.insert(0, Fortune{id: 0, message: 'Additional fortune added at request time.'})
	fortunes.sort(a.message < b.message)
	ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
	ctx.content_type = 'text/html; charset=utf-8'
	return $veb.html()
}

fn main() {
	mut app := &App{
		db: pg.connect(pg.Config{
			host:     'tfb-database'
			port:      5432
			user:     'benchmarkdbuser'
			password: 'benchmarkdbpass'
			dbname:   'hello_world'
		}) !
	}
	veb.run[App, Context](mut app,  8080)
}
