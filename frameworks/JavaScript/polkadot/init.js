const polkadot = require('polkadot');

polkadot((req, res) => {
	res.setHeader('Server', 'Polkadot');

	if (req.path === '/json') {
		res.setHeader('Content-Type', 'application/json');
		return { message: 'Hello, World!' };
	}

	if (req.path === '/plaintext') {
		return 'Hello, World!';
	}
}).listen(8080, '0.0.0.0', err => {
	if (err) throw err;
	console.log(`Worker started and listening on http://0.0.0.0:8080 ${new Date().toISOString()}`);
});
