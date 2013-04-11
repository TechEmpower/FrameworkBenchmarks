<?php defined('SYSPATH') OR die('No Direct Script Access');

Class Controller_Bench extends Controller
{
    public function action_json()
    {
        $this->response
            ->headers(array('Content-Type' => 'application/json'))
            ->body(json_encode(array("message" => "Hello World!")));
    }

    public function action_db()
    {
        $queries = $this->request->param('queries');
        $queries = (isset($queries) && is_numeric($queries))
            ? $queries
            : 1;

        $worlds = array();

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = DB::select()->from('World')
                ->where('id', '=', mt_rand(1, 10000))
                ->execute()
                ->current();
        }

        $this->response
            ->headers(array('Content-Type' => 'application/json'))
            ->body(json_encode($worlds));
    }
}