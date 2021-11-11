const { MongoClient } = require('mongodb');

let World, Fortune;
const projection = { _id:0 };

MongoClient.connect('mongodb://tfb-database:27017', { useNewUrlParser:true }, (err, ctx) => {
	const DB = ctx.db('hello_world');
	Fortune = DB.collection('fortune');
	World = DB.collection('world');
});

exports.fortunes = () => Fortune.find({}, { projection }).toArray();

exports.find = id => World.findOne({ id }, { projection });

exports.update = obj => World.replaceOne({ id:obj.id }, obj);
