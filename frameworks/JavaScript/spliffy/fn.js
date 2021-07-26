module.exports = {
    parseCount: ( i ) => Math.min( Math.max( parseInt( i, 10 ) || 1, 1 ), 500 ),
    randomId: () => Math.floor( Math.random() * 10000 ) + 1,
    randomUniqueIds: ( count ) => {
        const ids = {}
        for( let i = 0; i < count; i++ ) {
            let id = module.exports.randomId()
            if( ids[id] ) {
                for( let j = 0; j < 10000 - 1; j++ ) {
                    if( !ids[id] ) break
                    id++
                    if( id > 10000 ) {
                        id = 1
                    }
                }
            }
            ids[id] = true
        }
        return Object.keys( ids )
    },
}
