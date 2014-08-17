<?php

Class Test_Result implements Iterator
{

	public $pos = 0;
	public $data;

	public function key()
	{
		return $this->pos > count($this->data) - 1 ? count($this->data) - 1 : $this->pos;
	}

	public function valid()
	{
		return $this->pos < count($this->data);
	}

	public function next()
	{
		$this->pos++;
	}

	public function current()
	{
		return $this->data[$this->pos];
	}

	public function as_array()
	{
		return $this->data;
	}

	public function rewind()
	{

	}

}
