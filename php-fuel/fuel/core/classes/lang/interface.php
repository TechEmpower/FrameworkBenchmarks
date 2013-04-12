<?php

namespace Fuel\Core;

interface Lang_Interface
{
	public function load($overwrite = false);
	public function group();
	public function save($identifier, $contents);
}
