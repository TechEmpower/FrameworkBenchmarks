<?php
namespace models;

class Fortune {

	/**
	 *
	 * @id
	 * @column("name"=>"id","nullable"=>false,"dbType"=>"int(11)")
	 */
	public $id;

	/**
	 *
	 * @column("name"=>"message","nullable"=>false,"dbType"=>"varchar(100)")
	 */
	public $message;

	public function __construct($id = 0, $message = '') {
		$this->id = $id;
		$this->message = $message;
	}

	/**
	 *
	 * @param integer $id
	 * @return Fortune
	 */
	public function setId($id) {
		$this->id = $id;
		return $this;
	}

	/**
	 *
	 * @return integer
	 */
	public function getId() {
		return $this->id;
	}

	/**
	 *
	 * @param string $message
	 * @return Fortune
	 */
	public function setMessage($message) {
		$this->message = $message;
		return $this;
	}

	/**
	 *
	 * @return string
	 */
	public function getMessage() {
		return $this->message;
	}
}