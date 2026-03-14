module.exports = {

    sanititizeTotal : (total) => {
        return total > 500 ? 500 : (i | 0) || 1;
    },

    randomizeNum : () => {
        return ((Math.random() * 10000) | 0) + 1;
    }
}