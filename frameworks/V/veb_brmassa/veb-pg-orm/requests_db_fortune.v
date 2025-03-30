module main

import veb

struct Fortune {
	id      int @[primary; sql: serial]
	message string
}

// Fortunes page. It will automatically output using `fortunes.html`
pub fn (app &App) fortunes(mut ctx Context) veb.Result {
	fortunes_map := sql app.db {
		select from Fortune
	} or { return ctx.server_error('Database query failed') }

	mut fortunes := []Fortune{}
	for fortune in fortunes_map {
		fortunes << fortune
	}

	fortunes << Fortune{
		id:      0
		message: 'Additional fortune added at request time.'
	}

	fortunes.sort_with_compare(fn (a &Fortune, b &Fortune) int {
		return a.message.compare(b.message)
	})

	ctx.content_type = 'text/html; charset=UTF-8'
	// ctx.set_header(.content_type, content_type) // cannot be used since it's removing the "charset=UTF-8"
	return $veb.html()
}

fn db_init_fortune(app App, mut ctx Context) (int, string) {
	sql app.db {
		create table Fortune
	} or { return 1, 'Failed to create Fortune table: ${err}' }

	sql app.db {
		delete from Fortune where id > 0
	} or { return 1, 'Failed to clear Fortune table: ${err}' }

	mut messages := []string{}
	messages << 'fortune: No such file or directory'
	messages << 'A computer scientist is someone who fixes things that arent broken.' // TODO: escape the aren't
	messages << 'After enough decimal places, nobody gives a damn.'
	messages << 'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1'
	messages << 'A computer program does what you tell it to do, not what you want it to do.'
	messages << 'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen'
	messages << 'Any program that runs right is obsolete.'
	messages << 'A list is only as strong as its weakest link. — Donald Knuth'
	messages << 'Feature: A bug with seniority.'
	messages << 'Computers make very fast, very accurate mistakes.'
	messages << '<script>alert("This should not be displayed in a browser alert box.");</script>'
	messages << 'フレームワークのベンチマーク'

	mut i := 0
	for message in messages {
		i = i + 1
		fortune := Fortune{
			id:      i // unfortunately, the insertion in ORM will fail if the id is null
			message: message
		}
		sql app.db {
			insert fortune into Fortune
		} or { return 1, 'Failed to insert Fortune message: ${err}' }
	}

	return 0, 'Fortune table reset and filled with default rows'
}
