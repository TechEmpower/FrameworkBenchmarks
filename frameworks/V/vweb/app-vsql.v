module main

import vweb
import db.pg

const (
	http_port   = 8080
	http_host   = '127.0.0.1'
	db_host     = 'tfb-database'
	db_port     = 5432
	db_user     = 'benchmarkdbuser'
	db_password = 'benchmarkdbpass'
	db_name     = 'hello_world'
		// worldSelectSQL      = "SELECT id, randomNumber FROM World WHERE id = $1"
		// worldSelectCacheSQL = "SELECT id, randomNumber FROM World LIMIT $1"
		// worldUpdateSQL      = "UPDATE World SET randomNumber = $1 WHERE id = $2"
		// fortuneSelectSQL    = "SELECT id, message FROM Fortune"
)

type DBConnection = pg.DB

[table: 'world']
struct World {
	id            int [primary; sql: serial]
	random_number int [json: 'randomNumber']
}

[table: 'fortune']
struct Fortune {
	id      int    [primary; sql: serial]
	message string
}

struct App {
	vweb.Context
}

pub fn create_db_connection() !DBConnection {
	mut db := pg.connect(
		host: db_host
		port: db_port
		user: db_user
		password: db_password
		dbname: db_name
	)!

	return db
}

fn main() {
	mut db := create_db_connection() or { panic(err) }

	sql db {
		create table World
		create table Fortune
	}
	db.close()

	vweb.run_at(new_app(), vweb.RunParams{
		host: http_host
		port: http_port
		family: .ip
	}) or { panic(err) }
}

fn new_app() &App {
	mut app := &App{}

	return app
}

['/'; get]
pub fn (mut app App) ping() ?vweb.Result {
	return app.text('ping')
}

['/vsql-plaintext'; get]
pub fn (mut app App) plaintext() ?vweb.Result {
	return app.text('Hello, World!')
}

['/vsql-fortunes'; get]
pub fn (mut app App) fortunes() ?vweb.Result {
	return app.text('Hello, World!')
}

['/vsql-db'; get]
pub fn (mut app App) single_query() ?vweb.Result {
	return app.text('Hello, World!')
}

['/vsql-json'; get]
pub fn (mut app App) json() ?vweb.Result {
	return app.text('Hello, World!')
}

['/vsql-queries'; get]
pub fn (mut app App) multiple_queries() ?vweb.Result {
	return app.text('Hello, World!')
}

['/vsql-updates'; get]
pub fn (mut app App) update_queries() ?vweb.Result {
	return app.text('Hello, World!')
}

['/vsql-cached-worlds'; get]
pub fn (mut app App) cached_queries() ?vweb.Result {
	return app.text('Hello, World!')
}
