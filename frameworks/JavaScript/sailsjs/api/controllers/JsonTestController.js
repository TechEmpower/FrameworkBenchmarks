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
  get: function (req, res) {
    return res.json({
      message: 'Hello, World!'
    });
  }
};

