import veb
import time
import rand
import db.pg

import log

pub struct Context {
    veb.Context
}

pub struct App {
pub mut:
	db pg.DB
}

pub fn (app &App) plaintext(mut ctx Context) veb.Result {
	log.error('plain')
    ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
	return ctx.text('Hello, World!')
}

pub fn (app &App) json(mut ctx Context) veb.Result {
	log.error('json')
	obj := {'message': 'Hello, World!'}
	ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
    return ctx.json(obj)
}

struct World {
	id           int @[primary; sql: serial]
	randomnumber int
}

pub fn (app &App) db(mut ctx Context) veb.Result {
	r := rand.int_in_range(1, 10000) or { return ctx.text('rand error') }
	result := sql app.db {
		select from World where id == r
	} or { return ctx.text(app.db.last_id().str()) }
	ctx.set_header(.date, time.now().as_utc().custom_format('ddd, DD MMM YYYY HH:MM:ss') + ' GMT')
	ctx.set_header(.server, 'veb')
	return ctx.json({'id': r, 'randomNumber': result[0].randomnumber})
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
		//db: pg.connect_with_conninfo('postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?sslmode=disable') !
	}
	veb.run[App, Context](mut app,  8080)
}

/*
pub fn (app &App) queries(mut ctx Context) veb.Result {
	return 
}

pub fn (app &App) updates(mut ctx Context) veb.Result {
	return 
}

pub fn (app &App) fortunes(mut ctx Context) veb.Result {
	return 
}
*/
