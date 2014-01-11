<?php

use Yaf\Controller_Abstract as AbstractController;

class RawController extends AbstractController
{

    public function jsonAction ()
    {
        header('Content-type: application/json');
        die(json_encode(array('message' => 'Hello, World!')));
    }

    public function dbAction ()
    {
        $dbh = DatabaseManager::getInstance()->getConnection();

        $query_count = (int) $this->getRequest()->get('queries', 1);

        if (0 >= $query_count) {
            $query_count = 1;
        } elseif (500 < $query_count) {
            $query_count = 500;
        }

        $arr = array();
        $id = mt_rand(1, 10000);

        $statement = $dbh->prepare('SELECT `randomNumber` FROM `World` WHERE `id` = :id');
        $statement->bindParam(':id', $id, \PDO::PARAM_INT);

        while (0 < $query_count--) {
            $statement->execute();
            $arr[] = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
            $id = mt_rand(1, 10000);
        }

        if (count($arr) == 1) {
            $arr = $arr[0];
        }

        header('Content-type: application/json');
        die(json_encode($arr));
    }

    public function fortunesAction ()
    {
        $view = $this->getView();
        /* @var $view eYaf\Layout */

        $view->setLayout('fortunes');

        $dbh = DatabaseManager::getInstance()->getConnection();

        $statement = $dbh->query('SELECT `id`, `message` FROM `Fortune`');

        $arr = $statement->fetchAll(\PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';

        asort($arr);
        $view->rows = $arr;

        header('Content-Type: text/html; charset=utf-8');
    }

}