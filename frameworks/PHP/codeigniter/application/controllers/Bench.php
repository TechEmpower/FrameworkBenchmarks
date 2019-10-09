<?php
/**
 * Created by JetBrains PhpStorm.
 * User: Skamander
 * Date: 11.04.13
 * Time: 17:33
 * To change this template use File | Settings | File Templates.
 */

class Bench extends CI_Controller {

    public function plaintext() {
        $this->output
            ->set_content_type('text/plain')
            ->set_output('Hello, World!');
    } 

    public function json() {
        $this->output
            ->set_content_type('application/json')
            ->set_output(json_encode(array('message' => 'Hello, World!')));
    }

    public function db() {
        $worlds = $this->db
            ->query('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)))
            ->row();

        $this->output
            ->set_content_type('application/json')
            ->set_output(json_encode($worlds));
    }
    
    public function dbquery($queries = 1) {
        $worlds = array();
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $this->db
                ->query('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)))
                ->row();
        }

        $this->output
            ->set_content_type('application/json')
            ->set_output(json_encode($worlds));
    }

    public function fortunes() {
        $fortunes = $this->db
            ->query('SELECT * FROM Fortune')
            ->result_array();

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

        $this->load->view('fortunes', [
            'fortunes' => $fortunes
        ]);
    }
}
