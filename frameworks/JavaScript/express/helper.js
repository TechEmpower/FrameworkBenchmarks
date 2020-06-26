module.exports = {

    sanititizeTotal : (total) => {

        var totalIterations;
        if (!total || typeof(total) != 'number') {
            totalIterations = 1;
        } else if(total < 501 && total > 0) {
            totalIterations = total;
        } else if (total > 500) {
            totalIterations = 500;
        } else {
            totalIterations = 1;
        }
        return totalIterations;
    },

    randomizeNum : () => {

        return Math.floor(Math.random() * 10000) + 1
    }
}