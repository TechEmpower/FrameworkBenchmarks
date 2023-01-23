module main

import vweb
import db.sqlite // can change to 'db.mysql', 'db.pg'

const (
	http_port = 8080
	http_host = '127.0.0.1'
		// dbHost  = "tfb-database"
		// dbPort  = 5432
		// dbUser  = "benchmarkdbuser"
		// dbPaswd = "benchmarkdbpass"
		// dbName  = "hello_world"
		// worldSelectSQL      = "SELECT id, randomNumber FROM World WHERE id = $1"
		// worldSelectCacheSQL = "SELECT id, randomNumber FROM World LIMIT $1"
		// worldUpdateSQL      = "UPDATE World SET randomNumber = $1 WHERE id = $2"
		// fortuneSelectSQL    = "SELECT id, message FROM Fortune"
)

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

pub fn create_db_connection() !sqlite.DB {
	mut db := sqlite.connect('app.db')!
	return db
}

fn main() {
	mut db := create_db_connection() or { panic(err) }

	sql db {
		create table World
		create table Fortune
	}
	db.close()!

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

['/mongo-plaintext'; get]
pub fn (mut app App) plaintext() ?vweb.Result {
	return app.text('Hello, World!')
}

['/mongo-fortunes'; get]
pub fn (mut app App) fortunes() ?vweb.Result {
	return app.text('Hello, World!')
}

['/mongo-db'; get]
pub fn (mut app App) single_query() ?vweb.Result {
	return app.text('Hello, World!')
}

['/mongo-json'; get]
pub fn (mut app App) json() ?vweb.Result {
	return app.text('Hello, World!')
}

['/mongo-queries'; get]
pub fn (mut app App) multiple_queries() ?vweb.Result {
	return app.text('Hello, World!')
}

['/mongo-updates'; get]
pub fn (mut app App) update_queries() ?vweb.Result {
	return app.text('Hello, World!')
}

['/mongo-cached-worlds'; get]
pub fn (mut app App) cached_queries() ?vweb.Result {
	return app.text('Hello, World!')
}
