const { primaryKey, int } = require("chubbajs").database.annotations;
const Model = require("chubbajs").database.Model;

class World extends Model {

    constructor(id) {
        super(id);
    }

    @primaryKey
    id;

    @int
    randomnumber;

}

module.exports = World;
