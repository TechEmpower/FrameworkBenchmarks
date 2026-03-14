const postgres = require('postgres');

let sql;

module.exports = {
    async init() {
        sql = postgres({
            host: process.env.DB_HOST || 'tfb-database',
            user: 'benchmarkdbuser',
            password: 'benchmarkdbpass',
            database: 'hello_world',
            fetch_types: false,
            max: 1
        });
    },
    allFortunes: async () => await sql`SELECT id, message FROM fortune`,

    worldById: async (id) => await sql`SELECT id, randomNumber FROM world WHERE id = ${id}`.then((arr) => arr[0]),

    allWorlds: async () => await sql`SELECT id, randomNumber FROM world`,

    bulkUpdateWorld: async (worlds) => {
        const sortedWorlds = [...worlds].sort((a, b) => a.id - b.id);
        await sql`UPDATE world SET randomNumber = (update_data.randomNumber)::int
                  FROM (VALUES ${sql(sortedWorlds.map(w => [w.id, w.randomNumber]))}) AS update_data (id, randomNumber)
                  WHERE world.id = (update_data.id)::int`;
        return worlds;
    }
}