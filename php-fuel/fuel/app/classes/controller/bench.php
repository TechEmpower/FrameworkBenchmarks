<?php

class Controller_Bench extends Controller
{
    public function action_json()
    {
        return new Response(json_encode(array('message' => 'Hello, World!')), 200, array(
            'Content-Type' => 'application/json'
        ));
    }

    public function action_db()
    {
        $queries = Input::get('queries', 1);
        $worlds = array();

        for($i = 0; $i < $queries; ++$i) {
            $worlds[] = Model_World::find(mt_rand(1, 10000))->toJson();
        }

        return new Response(json_encode($worlds), 200, array(
            'Content-Type' => 'application/json'
        ));
    }

    public function action_fortunes()
    {
        $fortunes = Model_Fortune::find('all');

        $runtimeFortune = new Model_Fortune();
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

        return View::forge('bench/fortunes', [
            'fortunes' => $fortunes
        ]);
    }
}
