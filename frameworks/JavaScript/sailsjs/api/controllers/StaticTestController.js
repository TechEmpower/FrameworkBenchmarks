/**
 * JsonTestController
 *
 * @description :: Server-side logic for managing Jsontests
 * @help        :: See http://links.sailsjs.org/docs/controllers
 */

module.exports = {
	


  /**
   * Test 1: JSON Serialization
   */
  json: function (req, res) {
    return res.json({
      message: 'Hello, World!'
    });
  },

  /**
   * Test 6: Plaintext
   */
  plaintext: function (req, res) {
  	res.setHeader('Content-Type', 'text/plain');
  	return res.send("Hello, World!");
  }
   
};

