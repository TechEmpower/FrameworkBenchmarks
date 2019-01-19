module.exports = {
  randomTfbNumber: () => Math.floor(Math.random() * 10000) + 1,

  fillArray: (value, len) => {
    const filled = [];

    for (let i = 0; i < len; i++) {
      filled.push(value);
    }
    return filled;
  },

  getQueries: (req) => {
    let queries = ~~(req.query.queries) || 1;
    queries = Math.min(Math.max(queries, 1), 500);
    return queries;
  },

  additionalFortune: () => ({
    id: 0,
    message: 'Additional fortune added at request time.'
  })

};
