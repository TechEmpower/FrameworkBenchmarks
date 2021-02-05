<?php
//Include models
require_once 'model.php';

//Create your views here
class view extends Views {
  function __construct() {
    $this->db = DB('db');
  }

  function plaintext(Request $req) {
    $this->header('Content-type: text/plain');
    $this->response('Hello, World!');
  } 

  function json(Request $req) {
    $this->response_json(['message' => 'Hello, World!']);
  }

  function db(Request $req) {
   $sth = $this->db->prepare('SELECT * FROM World WHERE id = ?');
    $sth->execute(array(mt_rand(1, 10000)));
    $world = $sth->fetch(PDO::FETCH_ASSOC);
    # Cast fields to int so they don't get wrapped with quotes
    $world['id'] = (int) $world['id'];
    $world['randomNumber'] = (int) $world['randomNumber'];

    return $this->response_json($world);
  }

  function queries(Request $req) {
    $queries = $req->get->queries;
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }
    $query = $this->db->prepare('SELECT * FROM World WHERE id = ?');
    $worlds = array();
    for ($i = 0; $i < $queries; ++$i) {
        $query->execute(array(mt_rand(1, 10000)));
        $world = $query->fetch(PDO::FETCH_ASSOC);
        # Cast fields to int so they don't get wrapped with quotes
        $world['id'] = (int) $world['id'];
        $world['randomNumber'] = (int) $world['randomNumber'];
        $worlds[] = $world;
    }
    return $this->response_json($worlds);
  }

  function updates(Request $req) {
    $queries = $req->get->queries;
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }

    $sth = $this->db->prepare('SELECT * FROM World WHERE id = ?');
    $updateSth = $this->db->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');

    $worlds = array();
    for ($i = 0; $i < $queries; ++$i) {
        $id = mt_rand(1, 10000);
        $random_number = mt_rand(1, 10000);
        $sth->execute(array($id));
        $world = $sth->fetch(PDO::FETCH_ASSOC);
        # Cast fields to int so they don't get wrapped with quotes
        $world['id'] = (int) $world['id'];
        $world['randomNumber'] = $random_number;

        $updateSth->execute(array($world['randomNumber'], $world['id']));

        $worlds[] = $world;
    }
    return $this->response_json($worlds);
  }

  function fortunes(Request $req) {
    $fortunes = $this->db->query('SELECT * FROM Fortune')->fetchAll(PDO::FETCH_KEY_PAIR);
    $fortunes[0] = 'Additional fortune added at request time.';
    asort($fortunes);
    return $this->render('fortunes', ["fortunes" => $fortunes]);
  }

  function page_not_found(Request $req) {
    $this->response_code(404);
    return $this->render('errors/404');
  }

  function forbidden(Request $req) {
    $this->response_code(403);
    return $this->render('errors/403');
  }

  function internal_server_error(Request $req) {
    $this->response_code(500);
    return $this->render('errors/500');
  }
}
