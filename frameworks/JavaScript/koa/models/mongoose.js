const Bluebird = require('bluebird');
const Mongoose = require('mongoose');

Mongoose.Promise = Bluebird;

const { SchemaTypes } = Mongoose;

Mongoose.connect('mongodb://tfb-database/hello_world')
  .catch(error => {
    console.error(error);
    process.exit(1);
  });

const WorldSchema = new Mongoose.Schema({
  _id: SchemaTypes.Number,
  randomNumber: SchemaTypes.Number,
}, {
    collection: 'world',
    versionKey: false,
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