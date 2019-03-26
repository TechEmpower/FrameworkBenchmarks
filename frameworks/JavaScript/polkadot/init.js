const polkadot = require('polkadot');
const XSS = require('html-escaper');
const helper = require('./helper');

const { DATABASE } = process.env;
const DRIVER = DATABASE && require(`./drivers/${DATABASE}`);

polkadot(async (req, res) => {
	res.setHeader('Server', 'Polkadot');

	if (req.path === '/json') {
		res.setHeader('Content-Type', 'application/json');
		return { message: 'Hello, World!' };
	}

	if (req.path === '/plaintext') {
		return 'Hello, World!';
	}

	if (DRIVER) {
		if (req.path === '/db') {
			res.setHeader('Content-Type', 'application/json');
			return DRIVER.find(helper.random());
		}

		if (req.path === '/queries') {
			let arr=[], num=helper.parse(req.query.queries);
			while (num-- > 0) arr.push(DRIVER.find(helper.random()));
			res.setHeader('Content-Type', 'application/json');
			return Promise.all(arr);
		}

		if (req.path === '/fortunes') {
			const items = await DRIVER.fortunes();
			items.push(helper.fortune);

			items.sort((a, b) => a.message.localeCompare(b.message));

			let i=0, html='<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>';
			for (; i < items.length; i++) html += `<tr><td>${items[i].id}</td><td>${XSS.escape(items[i].message)}</td></tr>`;
			html += '</table></body></html>';

			res.setHeader('Content-Type', 'text/html; charset=utf-8');
			return html;
		}

		if (req.path === '/updates') {
			let reads=[], num=helper.parse(req.query.queries);
			while (num-- > 0) reads.push(DRIVER.find(helper.random()));
			const rows = await Promise.all(reads);

			let i=0, writes=[];
			for (; i < rows.length; i++) {
				rows[i].randomNumber = helper.random();
				writes.push(DRIVER.update(rows[i]));
			}

			await Promise.all(writes);
			res.setHeader('Content-Type', 'application/json');
			return rows;
		}
	}
}).listen(8080, '0.0.0.0', err => {
	if (err) throw err;
	console.log(`Worker started and listening on http://0.0.0.0:8080 ${new Date().toISOString()}`);
});
