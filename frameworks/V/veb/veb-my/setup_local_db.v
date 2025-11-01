import veb

// Populate the local database
pub fn (app &App) db_init(mut ctx Context) veb.Result {
	mut messages := []string{}
	mut error, mut msg := db_init_world(app, mut ctx)
	if error == 0 {
		messages << msg
	} else {
		return ctx.server_error(msg)
	}
	error, msg = db_init_fortune(app, mut ctx)
	if error == 0 {
		messages << msg
	} else {
		return ctx.server_error(msg)
	}

	return ctx.text(messages.join_lines())
}
