<?php

/* Example Column Options:
$column = array(
	'type' => 'primary|string|integer|boolean|decimal|datetime',
	'length' => NULL,
	'index' => FALSE,
	'null' => TRUE,
	'default' => NULL,
	'unique' => FALSE,
	'precision' => 0, // (optional, default 0) The precision for a decimal (exact numeric) column. (Applies only if a decimal column is used.)
	'scale' => 0, // (optional, default 0) The scale for a decimal (exact numeric) column. (Applies only if a decimal column is used.)
);
*/

$config = array(

	'test_table' => array(
		'id' => array('type' => 'primary'),
		'title' => array('type' => 'string', 'length' => 100),
		'text' => array('type' => 'string'),
		'created' => array('type' => 'datetime'),
		'modified' => array('type' => 'datetime'),
	),




);
