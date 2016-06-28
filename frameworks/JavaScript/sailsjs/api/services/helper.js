

module.exports = {

  randomTfbNumber: function() {
    return Math.floor(Math.random() * 10000) + 1
  },

  getQueries: function(req) {
    var queries = req.param('queries')
    queries = ~~(queries) || 1
    return Math.min(Math.max(queries, 1), 500)
  },

  ADDITIONAL_FORTUNE: {
    id: 0,
    message: "Additional fortune added at request time."
  }

}