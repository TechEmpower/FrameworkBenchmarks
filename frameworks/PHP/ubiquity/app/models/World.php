<?php

namespace models;

class World {
	
	/**
	 * @id
	 * @column("name"=>"id","nullable"=>false,"dbType"=>"int(11)")
	 **/
	private $id;
	
	/**
	 * @column("name"=>"randomNumber","nullable"=>false,"dbType"=>"int(11)")
	 **/
	private $randomNumber;
	
	/**
	 * @return mixed
	 */
	public function getId() {
		return $this->id;
	}

	/**
	 * @return mixed
	 */
	public function getRandomNumber() {
		return $this->randomNumber;
	}

	/**
	 * @param mixed $id
	 */
	public function setId($id) {
		$this->id = $id;
	}

	/**
	 * @param mixed $randomNumber
	 */
	public function setRandomNumber($randomNumber) {
		$this->randomNumber = $randomNumber;
	}

}

