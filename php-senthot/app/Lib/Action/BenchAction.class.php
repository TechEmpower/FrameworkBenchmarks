<?php
class BenchAction extends Action {

   public function rawjson() {
      header('Content-type: application/json');
      die(json_encode(array('message' => 'Hello World!')));
   }
   public function rawdb() {
      $query_count = (int) $this->_request('queries');

      if (0 >= $query_count) {
         $query_count = 1;
      } elseif (500 < $query_count) {
         $query_count = 500;
      }

      $arr = array();
      $World = M('World');

      while (0 < $query_count--) {
         $id = mt_rand(1, 10000);
         $d = $World->find($id);
         $arr[] = $d;
      }

      header('Content-type: application/json');
      die(json_encode($arr));

   }
   public function rawfortunes() {
      $Fortunes = M('Fortune');
      $data = $Fortunes->select();

      $data[] = array(
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        );

      usort($data, function($left, $right) {
         if ($left['message'] === $right['message']) {
            return 0;
         } else if ($left['message'] > $right['message']) {
            return 1;
         } else {
            return -1;
         }
      });

      $this->data = $data;
      $this->display();
   }
}