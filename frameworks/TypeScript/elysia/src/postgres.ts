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

export const bulkUpdate = (worlds: World[]) =>
	sql`UPDATE world SET randomNumber = (update_data.randomNumber)::int
	FROM (VALUES ${worlds
		.sort((a, b) => (a.id < b.id ? -1 : 1))
		.map((world) => [
			world.id,
			world.randomNumber,
		])}) AS update_data (id, randomNumber)
	WHERE world.id = (update_data.id)::int`;
