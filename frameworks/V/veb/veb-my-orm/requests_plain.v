import veb

pub fn (app &App) plaintext(mut ctx Context) veb.Result {
	return ctx.text('Hello, World!')
}

pub fn (app &App) json(mut ctx Context) veb.Result {
	data := {
		'message': 'Hello, World!'
	}
	return ctx.json(data)
}

// Home page. It will automatically output using `index.html`
pub fn (app &App) index(mut ctx Context) veb.Result {
	return $veb.html()
}
