<?php

namespace App\Model;

class Fortune extends \PHPixie\ORM\Model{
	public $table = 'Fortune';
	public $message;
		
	public function values($row, $set_loaded = false){
		parent::values($row, $set_loaded);
		$this->message = $this->_row['message'];
		return $this;
	}
}