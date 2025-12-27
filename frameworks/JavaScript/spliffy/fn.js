const parseCount = ( i ) => Math.min( Math.max( parseInt( i, 10 ) || 1, 1 ), 500 );

const randomId = function() { return Math.floor( Math.random() * 10000 ) + 1; };

const randomUniqueIds = ( count ) => {
    const used = new Map();
    const ids = [];
    for( let i = 0; i < count; i++ ) {
        let id = randomId(); // Use the locally defined randomId
        if( used.has(id) ) {
            for( let j = 0; j < 10000 - 1; j++ ) {
                if( !used.has(id) ) break;
                id++;
                if( id > 10000 ) {
                    id = 1;
                }
            }
        }
        used.set(id, true);
        ids.push(id);
    }
    return ids;
};

module.exports = {
    parseCount,
    randomId,
    randomUniqueIds
};
