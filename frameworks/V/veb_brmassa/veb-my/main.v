import veb
import time

pub struct Context {
	veb.Context
}

// Program entry point
fn main() {
	mut app := &App{}
	app.use(handler: header)

	db_init(mut app)
	defer {
		app.db.close()
	}

	veb.run[App, Context](mut app, 8080)
}

pub fn header(mut ctx Context) bool {
	ctx.set_header(.date, time.now().utc_string())
	ctx.set_header(.server, 'veb')
	return true
}
