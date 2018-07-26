const Mongoose = require('mongoose');

Mongoose.Promise = Promise;

const { SchemaTypes } = Mongoose;

Mongoose.connect('mongodb://tfb-database:27017/hello_world', { useNewUrlParser: true })
  .catch(error => {
    console.error(error);
    process.exit(1);
  });

const WorldSchema = new Mongoose.Schema({
  _id: Number,
  randomNumber: Number,
}, {
    collection: 'world',
    versionKey: false
  });

const FortuneSchema = new Mongoose.Schema({
  id: SchemaTypes.Number,
  message: SchemaTypes.String,
}, {
    collection: 'fortune',
    versionKey: false,
  });

module.exports.Worlds = Mongoose.model('World', WorldSchema, 'world');
module.exports.Fortunes = Mongoose.model('Fortune', FortuneSchema, 'fortune');