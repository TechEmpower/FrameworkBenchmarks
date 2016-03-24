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
        $queries = 1;
        $worlds = $this->getWorldsInfo($queries);
        $worlds = $worlds[0];
        header('Content-type: application/json');
        echo json_encode($worlds);
    }
    private function getWorldsInfo($queries)
    {
        $world = null;
        $worlds = $arr = array();
        for ($i = 0; $i < $queries; ++$i) {
            $world = World::find(mt_rand(1, 10000));
            $arr['id'] = (int) $world->id;
            $arr['randomNumber'] = (int) $world->randomnumber;
            $worlds[] = $arr;
        }
        return $worlds;
    }
    public function queriesAction($queries = 1)
    {
        $queries = intval($queries);
        if ($queries < 1 ) {
            $queries = 1;
        } elseif ($queries > 500) {
            $queries = 500;
        }
        $worlds = array();
        $worlds = $this->getWorldsInfo($queries);
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