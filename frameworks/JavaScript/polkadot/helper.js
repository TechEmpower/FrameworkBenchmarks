exports.random = () => Math.floor(Math.random() * 1e4) + 1;

exports.parse = queries => Math.min(Math.max(parseInt(queries, 10) || 1, 1), 500);

exports.fortune = {
	id: 0,
	message: 'Additional fortune added at request time.'
};
