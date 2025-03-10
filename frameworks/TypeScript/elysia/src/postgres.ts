import { SQL } from "bun";
import { rand } from "./db-handlers";
import type { Fortune, World } from "./types";

const sql = new SQL({
	url: "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world",
	max: 1,
});

export const fortunes = () =>
	sql`SELECT id, message FROM fortune`.execute() as Promise<Fortune[]>;

export const find = (id: number) =>
	sql`SELECT id, randomNumber FROM world WHERE id = ${id}`
		.execute()
		.then((arr) => arr[0]) as Promise<World[]>;

export const findThenRand = (id: number) =>
	sql`SELECT id, randomNumber FROM world WHERE id = ${id}`
		.execute()
		.then((arr) => {
			arr[0].randomNumber = rand();
			return arr[0];
		}) as Promise<World[]>;

export const bulkUpdate = (worlds: World[]) => {
	worlds = worlds.toSorted((a, b) => a.id - b.id);

	const values = new Array(worlds.length);
	for (let i = 0; i < worlds.length; i++)
		values[i] = [worlds[i].id, worlds[i].randomNumber];

	return sql`UPDATE world SET randomNumber = update_data.randomNumber
		FROM (VALUES ${sql(values)}) AS update_data (id, randomNumber)
		WHERE world.id = update_data.id`.execute();
};
