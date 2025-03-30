import veb
import os
import db.mysql as db_module

const db_connector = 'MySQL'
const db_connector_module = 'db.mysql'
const db_connector_port = 3306
const db_connector_varchar = 'TEXT' // Postgres and MySQL differ the text field (used in fortune)

@[heap]
pub struct App {
	veb.Middleware[Context]
pub mut:
	db db_module.DB
}

@[inline]
fn db_init(mut app App) {
	mut db_host := os.environ()['db_host']
	if db_host == '' {
		db_host = 'tfb-database'
	}

	config := db_module.Config{
		host:     db_host
		port:     db_connector_port
		username: 'benchmarkdbuser'
		password: 'benchmarkdbpass'
		dbname:   'hello_world'
	}
	app.db = db_module.connect(config) or { panic(err) }
	// or { return ctx.server_error('Database connection failed') }
}

// MySQL implementation return values while Postgres return options. Unify all as options
fn convert_option(id int, random_number ?string) World {
	return World{
		id:           id
		randomnumber: random_number or { '' }.int()
	}
}

// MySQL implementation return values while Postgres return options. Unify all as options
fn convert_fortune(id ?string, random_number ?string) Fortune {
	return Fortune{
		id:      id or { '' }.int()
		message: random_number or { '' }
	}
}
