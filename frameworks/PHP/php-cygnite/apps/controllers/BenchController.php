<?php
namespace Apps\Controllers;

use Cygnite\Mvc\Controller\AbstractBaseController;
use Apps\Models\Fortune;

class BenchController extends AbstractBaseController
{
    public function __construct()
    {
        parent::__construct();
    }

    public function indexAction()
    {
        echo 'Hello World!';
    }

    public function plaintextAction()
    {
        header("Content-Type: text/plain;");
        echo 'Hello, World!';
    }

    public function jsonAction()
    {
        header('Content-type: application/json');
        echo json_encode(array('message'=>'Hello, World!'));
    }

    public function dbAction()
    {

    }

    public function fortunesAction()
    {
        $allFortunes = array();
        $allFortunes = Fortune::all();
        $fortunes = $allFortunes->asArray();

        $runtimeFortune = new Fortune();
        $runtimeFortune->id = 0;
        $runtimeFortune->message = 'Additional fortune added at request time.';

        $fortunes[] = $runtimeFortune;

        usort($fortunes, function($left, $right) {

            if ($left->message === $right->message) {
                return 0;
            } else if ($left->message > $right->message) {
                return 1;
            } else {
                return -1;
            }

        });

        $this->render('fortunes', array(
                'fortunes' => $fortunes
        ));
    }
}