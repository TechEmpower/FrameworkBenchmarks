const { Client } = require('pg');

const client = new Client({
	host: 'tfb-database',
  user: 'benchmarkdbuser',
  password: 'benchmarkdbpass',
  database: 'hello_world'
});

client.connect();

function query(text, values) {
	return client.query(text, values || []).then(r => r.rows);
}

exports.fortunes = () => query('SELECT * FROM fortune');

exports.find = id => query('SELECT * FROM world WHERE id = $1', [id]).then(arr => arr[0]);

exports.update = obj => query('UPDATE world SET randomNumber = $1 WHERE id = $2', [obj.randomNumber, obj.id]);
