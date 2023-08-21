import { drizzle } from 'drizzle-orm/postgres-js';
import postgres from 'postgres';
import { eq } from 'drizzle-orm';
import { worlds, World, fortunes, Fortune } from './schema';

const connectionString = {
  host: process.env.NODE_ENV === 'production' ? 'tfb-database' : '0.0.0.0',
  user: 'benchmarkdbuser',
  password: 'benchmarkdbpass',
  database: 'hello_world',
  max: 1,
};

const client = postgres(connectionString);
const db = drizzle(client);

const allFortune = async (): Promise<Fortune[]> =>
  await db.select().from(fortunes);

const find = async (id: number): Promise<World> => {
  const [result] = await db.select().from(worlds).where(eq(worlds.id, id));
  return result;
};

const update = async (updatedWorld: World): Promise<World> => {
  const [result] = await db
    .update(worlds)
    .set({ randomnumber: updatedWorld.randomnumber })
    .where(eq(worlds.id, updatedWorld.id))
    .returning();
  return result;
};

export default {
  allFortune,
  find,
  update,
};
