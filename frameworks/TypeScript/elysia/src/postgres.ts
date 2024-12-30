import postgres from "postgres";
import { Fortune, World } from "./types";

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

export const bulkUpdate = (worlds: World[]) => {
  worlds = worlds.toSorted((a, b) => a.id - b.id);

  const values = new Array(worlds.length);
  for (let i = 0; i < worlds.length; i++) {
    values[i] = [worlds[i].id, worlds[i].randomNumber];
  }

  return sql`UPDATE world SET randomNumber = (update_data.randomNumber)::int
	FROM (VALUES ${sql(values)}) AS update_data (id, randomNumber)
	WHERE world.id = (update_data.id)::int`;
};
