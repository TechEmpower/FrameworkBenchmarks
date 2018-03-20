const Bluebird = require('bluebird');
const Mongoose = require('mongoose');

Mongoose.Promise = Bluebird;

const {SchemaTypes} = Mongoose;

Mongoose.connect('mongodb://TFB-database/hello_world')
  .catch(error => {
    console.error(error);
    process.exit(1);
  });

const WorldSchema = new Mongoose.Schema({
  id: SchemaTypes.Number,
  randomNumber: SchemaTypes.Number,
});

const FortuneSchema = new Mongoose.Schema({
  id: SchemaTypes.Number,
  message: SchemaTypes.String,
});

module.exports.Worlds = Mongoose.model('World', WorldSchema);
module.exports.Fortunes = Mongoose.model('Fortune', FortuneSchema);