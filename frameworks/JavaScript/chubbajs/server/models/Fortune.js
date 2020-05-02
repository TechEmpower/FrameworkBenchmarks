const { primaryKey, varChar } = require("chubbajs").database.annotations;
const Model = require("chubbajs").database.Model;

class Fortune extends Model {

    constructor(id) {
        super(id);
    }

    @primaryKey
    id;

    @varChar(200)
    message;

}

module.exports = Fortune;
