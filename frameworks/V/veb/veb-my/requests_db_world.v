module main

import rand
import veb
import arrays

struct World {
	id int @[primary; sql: serial]
mut:
	randomnumber int
}

pub fn (app &App) db(mut ctx Context) veb.Result {
	id := rand.int_in_range(1, 10001) or { panic(err) }
	world := get_world(app, id) or { return ctx.server_error('${err}') }
	return ctx.json(world)
}

pub fn (app &App) queries(mut ctx Context) veb.Result {
	queries := get_query(ctx, 'q')

	mut worlds := []World{}
	for _ in 0 .. queries {
		id := rand.int_in_range(1, 10001) or { panic(err) }
		world := get_world(app, id) or { return ctx.server_error('${err}') }
		worlds << world
	}

	return ctx.json(worlds)
}

@[inline]
fn get_world(app &App, id int) !World {
	world_map := app.db.exec_one('SELECT id, randomNumber FROM ${db_table_world} WHERE id = ${id}') or {
		return error('Failed to fetch world: ${err}')
	}

	random_number := world_map.vals[1]
	world := convert_option(id, random_number)

	return world
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

pub fn (app &App) update(mut ctx Context) veb.Result {
	queries := get_query(ctx, 'q')

	mut worlds := []World{}
	for _ in 0 .. queries {
		id := rand.int_in_range(1, 10001) or { panic(err) }
		world := get_world(app, id) or { return ctx.server_error('${err}') }
		worlds << world
	}

	for mut world in worlds {
		world.randomnumber = rand.int_in_range(1, 10001) or { panic(err) }
		// MySQL implementation fails to use exec_param_many, so I've opted to interpolate manually
		app.db.exec('UPDATE ${db_table_world} SET randomNumber = ${world.randomnumber} WHERE id = ${world.id}') or {
			return ctx.server_error('Database update failed: ${err}')
		}
		// app.db.exec_param_many('UPDATE ${db_table_world} SET randomNumber = $1 WHERE id = $2',
		// 	[
		// 	world.randomnumber.str(),
		// 	world.id.str(),
		// ]) or { return ctx.server_error('Database update failed: ${err}') }
	}

	return ctx.json(worlds)
}

fn db_init_world(app App, mut ctx Context) (int, string) {
	app.db.exec('DROP TABLE IF EXISTS ${db_table_world}') or {
		return 1, 'Failed to drop existing world table: ${err}'
	}
	app.db.exec('CREATE TABLE ${db_table_world} (
		id SERIAL PRIMARY KEY,
		randomNumber INT NOT NULL
	)') or {
		return 1, 'Failed to create world table: ${err}'
	}

	mut random_numbers := []int{}
	for _ in 0 .. 10000 {
		random_numbers << rand.int_in_range(1, 10001) or {
			return 1, 'Random number generation failed: ${err}'
		}
	}
	numbers := arrays.join_to_string(random_numbers, '),(', fn (it int) string {
		return it.str()
	})
	app.db.exec('INSERT INTO ${db_table_world} (randomNumber) VALUES (${numbers})') or {
		return 1, 'Failed to insert world values: ${err}'
	}

	return 0, 'World table reset and filled with 10000 rows'
}
