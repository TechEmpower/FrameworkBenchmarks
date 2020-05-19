<?php

class Controller_Bench extends Controller
{

    private function getUniqueRandomNumbers($count, $min, $max)
    {
        $res = array();
        do {
            $res[\mt_rand($min, $max)] = 1;
        } while (\count($res) < $count);
        return \array_keys($res);
    }

    public function action_json()
    {
        return new Response(json_encode(array(
            'message' => 'Hello, World!'
        )), 200, array(
            'Content-Type' => 'application/json'
        ));
    }

    public function action_db()
    {
        $worlds = Model_World::find(mt_rand(1, 10000))->toJson();

        return new Response(json_encode($worlds), 200, array(
            'Content-Type' => 'application/json'
        ));
    }

    public function action_dbquery()
    {
        $queries = Input::get('queries', 1);
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;

        $worlds = array();
        $numbers = $this->getUniqueRandomNumbers($queries, 1, 10000);
        foreach ($numbers as $id) {
            $worlds[] = Model_World::find($id)->toJson();
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

        usort($fortunes, function ($left, $right) {
            if ($left->message === $right->message) {
                return 0;
            } else if ($left->message > $right->message) {
                return 1;
            } else {
                return - 1;
            }
        });

        return View::forge('bench/fortunes', [
            'fortunes' => $fortunes
        ]);
    }
}
