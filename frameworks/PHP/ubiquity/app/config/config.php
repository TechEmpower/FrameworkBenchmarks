<?php
return array(
		"database"=>[
				"type"=>"mysql",
				"dbName"=>"hello_world",
				"serverName"=>"127.0.0.1",//tfbdata
				"port"=>"3306",
				"user"=>"root",//benchmarkdbuser
				"password"=>"",//benchmarkdbpass
				"options"=>[PDO::ATTR_PERSISTENT => true],
				"cache"=>false
		],
		"test"=>false,
		"debug"=>false,
		"cache"=>["directory"=>"cache/","system"=>"Ubiquity\\cache\\system\\ArrayCache","params"=>[]],
		"mvcNS"=>["models"=>"models","controllers"=>"controllers","rest"=>""]
);
