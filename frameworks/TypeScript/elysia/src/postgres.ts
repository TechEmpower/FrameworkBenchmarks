import postgres from "postgres";
import { rand } from "./db-handlers";
import type { Fortune, World } from "./types";

const sql = postgres({
	host: "tfb-database",
	user: "benchmarkdbuser",
	password: "benchmarkdbpass",
	database: "hello_world",
	max: 1,
});

export const fortunes = () => sql<Fortune[]>`SELECT id, message FROM fortune`;

export const find = (id: number) =>
	sql<World[]>`SELECT id, randomNumber FROM world WHERE id = ${id}`.then(
		(arr) => arr[0],
	);

export const findThenRand = (id: number) =>
	sql<World[]>`SELECT id, randomNumber FROM world WHERE id = ${id}`.then(
		(arr) => {
			arr[0].randomNumber = rand();
			return arr[0];
		},
	);

export const bulkUpdate = (worlds: World[]) => {
	const sorted = worlds.toSorted((a, b) => a.id - b.id);

	const values = new Array(sorted.length);
	for (let i = 0; i < sorted.length; i++)
		values[i] = [sorted[i].id, sorted[i].randomNumber];

	sql`UPDATE world SET randomNumber = (update_data.randomNumber)::int
		FROM (VALUES ${values}) AS update_data (id, randomNumber)
		WHERE world.id = (update_data.id)::int`;
};
