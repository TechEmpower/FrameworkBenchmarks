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
    $this->jsonAction();
  }

  /**
   * Test 1: JSON serialization
   */
  public function jsonAction()
  {
    $this->response->asJSON()->send(array("message" => "Hello World!"));
  }

  /**
   * Test 2: Multiple database queries
   */
  public function queriesAction()
  {
    $queries = max(1, min($this->request->fromGet()->get('queries', 1), 500));

    $worlds = array();

    for ($i = 0; $i < (int)$queries; ++$i) {
      $worlds[] = Registry::get('em')->world->find(mt_rand(1, 10000));
    }

    $this->response->asJSON()->send($worlds);
  }

  /**
   * Test 3: Single database query
   */
  public function dbAction()
  {
    $worlds = Registry::get('em')->world->find(1);

    $this->response->asJSON()->send($worlds);
  }

  /**
   * Test 4: Fortunes
   */
  public function fortunesAction()
  {
    $templates = array();
    $fortunes = Registry::get('em')->fortune->getAll();
    $fortunes[] = 'Additional fortune added at request time.';

    asort($fortunes);

    foreach ($fortunes as $i => $fortune) {
        $templates[$i] = '<tr><td>'.$i.'</td><td>'.$fortune.'</td></tr>';
    }

    $this->response->asHTML()->send(
      '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'.implode('', $templates).'</table></body></html>'
    );
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

    for ($i = 0; $i < (int)$queries; ++$i) {
      $worlds[] = $em->world->find(mt_rand(1, 10000));
    }

    foreach ($worlds as $i => $row) {
      $row[$i]['randomNumber'] = rand(1, 10000);
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
    $this->response->asTEXT()->send('Hello World!');
  }
}