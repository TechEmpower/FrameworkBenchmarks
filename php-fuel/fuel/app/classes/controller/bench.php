<?php

class Controller_Bench extends Controller
{
    public function action_json()
    {
        return new Response(json_encode(array('message' => 'Hello World!')), 200, array(
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
}
