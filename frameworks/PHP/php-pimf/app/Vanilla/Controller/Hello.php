<?php
namespace Vanilla\Controller;

use Pimf\Controller\Base, Pimf\View, Pimf\Registry;

class Hello extends Base
{
  /**
   * A index action - this is a framework restriction!
   */
  public function indexAction()
  {
    $this->plaintextAction();
  }

  /**
   * Test 1: JSON serialization
   */
  public function jsonAction()
  {
    $this->response->asJSON()->send(array("message" => "Hello, World!"));
  }

  /**
   * Test 2: Multiple database queries
   */
  public function queriesAction()
  {
    $queries = max(1, min($this->request->fromGet()->get('queries', 1), 500));

    $worlds = array();

    for ($i = 0; $i < $queries; ++$i) {
      $worlds[] = Registry::get('em')->world->find(mt_rand(1, 10000));
    }

    $this->response->asJSON()->send($worlds);
  }

  /**
   * Test 3: Single database query
   */
  public function dbAction()
  {
    $worlds = Registry::get('em')->world->find(mt_rand(1, 10000));

    $this->response->asJSON()->send($worlds);
  }

  private static function my_cmp($a, $b)
  {
    return  strcmp($a["message"], $b["message"]);
  }

  /**
   * Test 4: Fortunes
   */
  public function fortunesAction()
  {
    $fortunes = Registry::get('em')->fortune->getAll();

    $fortunes[] = array(
      'id' => 0,
      'message' => 'Additional fortune added at request time.'
    );

    usort($fortunes, array($this, "my_cmp"));

    $view = new View('table.phtml', array('fortunes' => $fortunes));

    $this->response->asHTML()->send($view);
  }

  /**
   * Test 5: Database updates
   */
  public function updatesAction()
  {
    $queries = max(1, min($this->request->fromGet()->get('queries', 1), 500));

    $worlds = array();

    /* @var $em \Pimf\EntityManager */
    $em = Registry::get('em');

    $em->beginTransaction();

    for ($i = 0; $i < $queries; ++$i) {
      $worlds[] = $em->world->find(mt_rand(1, 10000));
    }

    foreach ($worlds as $row) {
      $row['randomNumber'] = rand(1, 10000);
      $em->world->update($row);
    }

    $em->commitTransaction();

    $this->response->asJSON()->send($worlds);
  }

  /**
   * Test 6: Plaintext
   */
  public function plaintextAction()
  {
    $this->response->asTEXT()->send('Hello, World!');
  }
}
