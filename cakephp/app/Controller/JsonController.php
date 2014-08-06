<?php
//
// JSON Encoding Test
//
class JsonController extends AppController {

  // Needed to enable JsonView
  // http://book.cakephp.org/2.0/en/views/json-and-xml-views.html#enabling-data-views-in-your-application
  public $components = array('RequestHandler');

  public function index() {
    // Use the CakePHP JSON View
    // http://book.cakephp.org/2.0/en/views/json-and-xml-views.html
    $this->set('message', "Hello, World!");
    $this->set('_serialize', array('message'));
  }
}
?>