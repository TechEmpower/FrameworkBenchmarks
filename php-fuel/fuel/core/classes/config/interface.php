<?php

namespace Fuel\Core;

interface Config_Interface
{
	public function load($overwrite = false);
	public function group();
	public function save($contents);
}
