module.exports = {
    parseCount: ( i ) => Math.min( Math.max( parseInt( i, 10 ) || 1, 1 ), 500 )
}