module.exports = {
  randomTfbNumber: () => Math.floor(Math.random() * 10000) + 1,

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
