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
    $should_return_array = True;
    if ($query_count == null) {
      $query_count = 1;
      $should_return_array = False;
    }
    $query_count = intval($query_count);
    if ($query_count == 0) {
      $query_count = 1;
    } elseif ($query_count > 500) {
      $query_count = 500;
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
      // Cast numbers to int so they don't get quoted in JSON
      $result = $world['World'];
      $result['id'] = (int) $result['id'];
      $result['randomNumber'] = (int) $result['randomNumber'];
      // Store result in array.
      $arr[] = array("id" => $result['id'], "randomNumber" => $result['randomNumber']);
    }

    # Return either one object or a json list
    if ($should_return_array == False) {
      $this->set('worlds', $arr[0]);
    } else {
      $this->set('worlds', $arr);  
    }

    // Use the CakePHP JSON View
    // http://book.cakephp.org/2.0/en/views/json-and-xml-views.html
    $this->set('_serialize', 'worlds');
  }

}
?>

