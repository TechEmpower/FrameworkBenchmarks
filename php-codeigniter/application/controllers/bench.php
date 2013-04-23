<?php
/**
 * Created by JetBrains PhpStorm.
 * User: Skamander
 * Date: 11.04.13
 * Time: 17:33
 * To change this template use File | Settings | File Templates.
 */

class Bench extends CI_Controller {

    public function json() {
        $this->output
            ->set_content_type('application/json')
            ->set_output(json_encode(array('message' => 'Hello World!')));
    }

    public function db($queries = 1) {
        $worlds = array();

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $this->db
                ->query('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)))
                ->row();
        }

        $this->output
            ->set_content_type('application/json')
            ->set_output(json_encode($worlds));
    }
}