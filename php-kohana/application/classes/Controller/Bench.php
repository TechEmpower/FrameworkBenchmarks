<?php defined('SYSPATH') OR die('No Direct Script Access');

Class Controller_Bench extends Controller
{
    public function action_json()
    {
        $this->response
            ->headers(array('Content-Type' => 'application/json'))
            ->body(json_encode(array('message' => 'Hello, World!')));
    }

    public function action_db()
    {
        $queries = $this->request->param('queries', false);
        $queries = $queries
                    ? $queries
                    : 1;

        $worlds = array();

        $query = DB::query(Database::SELECT, 'SELECT * FROM World WHERE id = :id')->bind(':id', $id);

        for ($i = 0; $i < $queries; $i++) {
            $worlds[] = $query->param(':id', mt_rand(1, 10000))->execute()->current();
        }

        if ($queries == 1) {
            $worlds = $worlds[0];
        }

        $this->response
            ->headers(array('Content-Type' => 'application/json'))
            ->body(json_encode($worlds));
    }

    public function action_fortunes()
    {
        $fortunes = DB::select()->from('Fortune')
            ->execute()
            ->as_array();

        $fortunes[] = array(
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        );

        usort($fortunes, function($left, $right) {
            if ($left['message'] === $right['message']) {
                return 0;
            } else if ($left['message'] > $right['message']) {
                return 1;
            } else {
                return -1;
            }
        });

        $this->response->body(
            View::factory('bench/fortunes')
                ->bind('fortunes', $fortunes)
                ->render()
        );
    }
}
