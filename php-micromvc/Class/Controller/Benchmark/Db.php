<?php

namespace Controller\Benchmark;

use Model\Benchmark\World;

class Db extends \MyController
{
    public function run()
    {
        $queries = get('queries', 1);
        $this->worlds = array();
        $this->load_database();

        for ($i = 0; $i < $queries; ++$i) {
            $this->worlds[] = World::row(array('id' => mt_rand(1, 10000)))->to_array();
        }

        if ($queries == 1) {
            $this->worlds = $this->worlds[0];
        }
    }

    public function send() {
        headers_sent() OR header('Content-type: application/json');
        echo json_encode($this->worlds);
    }
}
