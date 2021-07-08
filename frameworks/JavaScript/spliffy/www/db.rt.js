const db = require( '../db' )

module.exports = {
    GET: () => db.findWorldById( db.randomId() )
}