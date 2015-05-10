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

    public function dbAction()
    {
        $worlds = $arr = array();
        $world = null;
        $queries = (!is_null(Url::segment('3'))) ? intval(Url::segment('3')) : intval(1);

        if ($queries < 1) {
            $queries = 1;
        } elseif ($queries > 500) {
            $queries = 500;
        }

        for ($i = 0; $i < $queries; ++$i) {
            $world = World::find(mt_rand(1, 10000));
            $arr['id'] = (int) $world->id;
            $arr['randomNumber'] = (int) $world->randomnumber;
            $worlds[] = $arr;
        }

        header('Content-type: application/json');
        echo json_encode($worlds);
    }

    public function fortunesAction()
    {
        $fortuneCollection = array();
        $fortuneCollection = Fortune::all();
        $fortunes = $fortuneCollection->asArray();

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