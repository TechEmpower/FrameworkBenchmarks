<?php
namespace Apps\Controllers;

use Cygnite\Mvc\Controller\AbstractBaseController;
use Apps\Models\Fortune;
use Apps\Models\World;

class BenchController extends AbstractBaseController
{
    protected $templateEngine = false;

    public function __construct()
    {
        parent::__construct();
    }

    public function indexAction()
    {
        echo 'Hello World!';
    }

    public function dbAction($queries = 1)
    {
        $worlds = array();
        $world = null;

        for ($i = 0; $i < $queries; ++$i) {
            $world = World::find(mt_rand(1, 10000));
            $worlds[] = $world->getAttributes();
        }


        if ($queries == 1) {
            $worlds = $worlds[0];
        }

        header('Content-type: application/json');
        echo json_encode($worlds);
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