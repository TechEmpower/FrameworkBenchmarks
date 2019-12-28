/**
 * Passes the proper config object to the application
 */

let config = {};

try {
  config = require(`./config.${process.env.NODE_ENV}.js`);
} catch(e) {
  console.log('No config for this environment exists.');
}

module.exports = config;
