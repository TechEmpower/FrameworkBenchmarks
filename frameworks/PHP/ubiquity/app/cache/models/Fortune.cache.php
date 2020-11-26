<?php
return array(
	"#tableName" => "fortune",
	"#primaryKeys" => array(
		"id" => "id"
	),
	"#manyToOne" => array(),
	"#fieldNames" => array(
		"id" => "id",
		"message" => "message"
	),
	"#memberNames" => array(
		"id" => "id",
		"message" => "message"
	),
	"#fieldTypes" => array(
		"id" => "int(11)",
		"message" => "varchar(100)"
	),
	"#nullable" => array(),
	"#notSerializable" => array(),
	"#transformers" => array(),
	"#accessors" => array(
		"id" => "setId",
		"message" => "setMessage"
	)
);
