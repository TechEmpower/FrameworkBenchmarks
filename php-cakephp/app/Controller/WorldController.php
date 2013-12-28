<?php
//
// Database Mapping Test
//
class WorldController extends AppController {

  // Needed to enable JsonView
  // http://book.cakephp.org/2.0/en/views/json-and-xml-views.html#enabling-data-views-in-your-application
  public $components = array('RequestHandler');

  public function index() {

    // Read number of queries to run from URL parameter
    // http://book.cakephp.org/2.0/en/controllers/request-response.html#accessing-request-parameters
    $query_count = $this->request->query('queries');
    if ($query_count == null) {
      $query_count = 1;
    }

    // Create an array with the response string.
    $arr = array();

    // For each query, store the result set values in the response array
    for ($i = 0; $i < $query_count; $i++) {
      // Choose a random row
      // http://www.php.net/mt_rand
      $id = mt_rand(1, 10000);

      // Retrieve a model by ID
      // http://book.cakephp.org/2.0/en/models/retrieving-your-data.html#find
      $world = $this->World->find('first', array('conditions' => array('id' => $id)));
      // Store result in array.
      $arr[] = array("id" => $world['World']['id'], "randomNumber" => $world['World']['randomNumber']);
    }

    // Use the CakePHP JSON View
    // http://book.cakephp.org/2.0/en/views/json-and-xml-views.html
    $this->set('worlds', $arr);
    $this->set('_serialize', array('worlds'));
  }
}
?>

