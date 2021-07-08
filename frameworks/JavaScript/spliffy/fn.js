module.exports = {
    parseIntBetween: ( i, min, max ) => Math.min( Math.max( parseInt( i, 10 ) || 1, min ), max )
}