module.exports = {
  randomTfbNumber: () => Math.floor(Math.random() * 10000) + 1,

  getQueries: (queries) => {
    return Math.min(Math.max(parseInt(queries) || 1, 1), 500);
  },

  additionalFortune: {
    id: 0,
    message: 'Additional fortune added at request time.'
  }
};
