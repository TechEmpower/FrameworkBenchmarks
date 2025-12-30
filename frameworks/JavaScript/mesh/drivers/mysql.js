const { createConnection } = require('mysql');

const connection = createConnection({
	host: 'tfb-database',
	user: 'benchmarkdbuser',
	password: 'benchmarkdbpass',
	database: 'hello_world'
});

connection.connect();

function query(text, values) {
	return new Promise((res, rej) => {
		connection.query(text, values || [], (err, results) => {
			return err ? rej(err) : res(results);
		});
	});
}

exports.fortunes = () => query('SELECT * FROM fortune');

exports.find = id => query('SELECT * FROM world WHERE id = ?', [id]).then(arr => arr[0]);

exports.update = obj => query('UPDATE world SET randomNumber = ? WHERE id = ?', [obj.randomNumber, obj.id]);
