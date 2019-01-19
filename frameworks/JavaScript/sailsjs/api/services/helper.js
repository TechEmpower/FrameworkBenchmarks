

module.exports = {

  randomTfbNumber: () => Math.floor(Math.random() * 10000) + 1,

  getQueries: (req) => {
    let queries = req.param('queries');
    queries = ~~(queries) || 1;
    return Math.min(Math.max(queries, 1), 500)
  },

  additionalFortune: () => ({
    id: 0,
    message: "Additional fortune added at request time."
  })

};
