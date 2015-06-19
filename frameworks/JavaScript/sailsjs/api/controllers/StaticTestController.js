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
  Json: function (req, res) {
    return res.json({
      message: 'Hello, World!'
    });
  },

  /**
   * Test 6: Plaintext
   */
  Plaintext: function (req, res) {
  	res.setHeader('Content-Type', 'text/plain');
  	return res.send("Hello, World!");
  }
   
};

