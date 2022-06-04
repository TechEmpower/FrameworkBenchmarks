<?php
return array(
	"database" => [
		'mysql' => [
			"wrapper" => "\\Ubiquity\\db\\providers\\pdo\\PDOWrapper",
			"type" => "mysql",
			"dbName" => "hello_world",
			"serverName" => "tfb-database", // tfb-database
			"port" => 3306,
			"user" => "benchmarkdbuser", // benchmarkdbuser
			"password" => "benchmarkdbpass", // benchmarkdbpass
			"options" => [
				\PDO::ATTR_EMULATE_PREPARES => false
			],
			"cache" => false
		],
		'pgsql' => [
			"wrapper" => "\\Ubiquity\\db\\providers\\pdo\\PDOWrapper",
			"type" => "pgsql",
			"dbName" => "hello_world",
			"serverName" => "tfb-database", // tfb-database
			"port" => 5432,
			"user" => "benchmarkdbuser", // benchmarkdbuser
			"password" => "benchmarkdbpass", // benchmarkdbpass
			"options" => [
				\PDO::ATTR_EMULATE_PREPARES => false,
				'quote' => '',
				\PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC
			],
			"cache" => false
		],
		'pgsql-cache' => [
			"wrapper" => "\\Ubiquity\\db\\providers\\pdo\\PDOWrapper",
			"type" => "pgsql",
			"dbName" => "hello_world",
			"serverName" => "tfb-database", // tfb-database
			"port" => 5432,
			"user" => "benchmarkdbuser", // benchmarkdbuser
			"password" => "benchmarkdbpass", // benchmarkdbpass
			"options" => [
				'quote' => ''
			],
			"cache" => false
		],
		'mongo' => [
			"wrapper" => "\\Ubiquity\\db\\providers\\MongoDbWrapper",
			"type" => "mongo",
			"dbName" => "hello_world",
			"serverName" => "tfb-database", // tfb-database
			"port" => 27017,
			"user" => "", // benchmarkdbuser
			"password" => "", // benchmarkdbpass
			"options" => [],
			"cache" => false
		]
	],
	"test" => false,
	"debug" => false,
	"cache" => [
		"directory" => "cache/",
		"system" => "Ubiquity\\cache\\system\\ArrayCache",
		"params" => []
	],
	"mvcNS" => [
		"models" => "models",
		"controllers" => "controllers",
		"rest" => ""
	]
);
