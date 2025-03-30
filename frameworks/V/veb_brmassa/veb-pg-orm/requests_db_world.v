module main

import rand
import veb

// required in ORM scenarios because it tries to lowercase the table name
// @[table: 'World']
struct World {
	id int @[primary; sql: serial]
mut:
	randomnumber int
}

pub fn (app &App) db(mut ctx Context) veb.Result {
	id := rand.int_in_range(1, 10001) or { panic(err) }
	world := sql app.db {
		select from World where id == id
	} or { return ctx.server_error('Failed to fetch world') }

	return ctx.json(world.first())
}

pub fn (app &App) queries(mut ctx Context) veb.Result {
	queries := get_query(ctx, 'q')

	mut worlds := []World{}
	for _ in 0 .. queries {
		id := rand.int_in_range(1, 10001) or { panic(err) }

		world := sql app.db {
			select from World where id == id
		} or { return ctx.server_error('Failed to fetch world') }
		worlds << world
	}

	return ctx.json(worlds)
}

pub fn (app &App) update(mut ctx Context) veb.Result {
	queries := get_query(ctx, 'q')

	mut worlds := []World{}
	for _ in 0 .. queries {
		id := rand.int_in_range(1, 10001) or { panic(err) }
		world := sql app.db {
			select from World where id == id
		} or { return ctx.server_error('Failed to fetch world') }
		worlds << world
	}

	for mut world in worlds {
		world.randomnumber = rand.int_in_range(1, 10001) or { panic(err) }
		sql app.db {
			update World set randomnumber = world.randomnumber where id == world.id
		} or { return ctx.server_error('Database update failed') }
	}

	return ctx.json(worlds)
}

@[inline]
fn get_query(ctx Context, index string) int {
	mut queries := ctx.query[index].int()
	return if queries < 1 {
		1
	} else if queries > 500 {
		500
	} else {
		queries
	}
}

fn db_init_world(app App, mut ctx Context) (int, string) {
	sql app.db {
		create table World
	} or { return 1, 'Failed to create World table: ${err}' }

	sql app.db {
		delete from World where id > 0
	} or { return 1, 'Failed to clear World table: ${err}' }

	for i in 0 .. 10000 {
		world := World{
			id:           i + 1 // unfortunately, the insertion in ORM will fail if the id is null
			randomnumber: rand.int_in_range(1, 10001) or { 1 }
		}

		// ORM only insert one at a time
		sql app.db {
			insert world into World
		} or { return 1, 'Failed to insert World values: ${err}' }
	}

	return 0, 'World table reset and filled with 10000 rows'
}
