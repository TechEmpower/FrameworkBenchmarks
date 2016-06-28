<?php

namespace Controller;

class Index extends \MyController
{
	public function run()
	{
		// Load database
		//$this->db = new DB(config('database'));

		// Set ORM database connection
		//ORM::$db = $this->db;

		// Load the theme sidebar since we don't need the full page
		$this->sidebar = new \Micro\View('Sidebar');

		// Load the welcome view
		$this->content = new \Micro\View('Index/Index');
	}
}
