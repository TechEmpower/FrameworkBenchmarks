const { createPool } = require("mariadb");

const clientOpts = {
  host: process.env.MYSQL_HOST,
  user: process.env.MYSQL_USER,
  password: process.env.MYSQL_PSWD,
  database: process.env.MYSQL_DBNAME,
};

const pool = createPool({ ...clientOpts, connectionLimit: 1 });
const execute = (text, values) => pool.execute(text, values || undefined);

async function allFortunes() {
  return execute("select id, message from fortune", []);
}

async function getWorld(id) {
  return execute("select id, randomNumber from world where id = ?", [id]).then(
    (arr) => arr[0]
  );
}

async function bulkUpdate(worlds) {
  const sql = "update world set randomNumber = ? where id = ?";
  const values = worlds
    .map((world) => [world.randomnumber, world.id])
    .sort((a, b) => (a[0] < b[0] ? -1 : 1));
  return pool.batch(sql, values);
}

module.exports = {
  getWorld,
  bulkUpdate,
  allFortunes,
};
