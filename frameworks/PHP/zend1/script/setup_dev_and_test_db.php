<?php

set_include_path(realpath(dirname(__FILE__) . '/../vendor/zendframework/zendframework1/library'));

require_once 'Zend/Db/Adapter/Pdo/Mysql.php';
$db = new Zend_Db_Adapter_Pdo_Mysql(array(
    'host' => 'localhost',
    'charset' => 'utf8',
    'dbname'   => 'hello_world',
    'username' => 'benchmarkdbuser',
    'password' => 'benchmarkdbpass'
));

$db->exec('DROP TABLE IF EXISTS `World`;');
$db->exec('CREATE TABLE `World` (
  `id` int(11) NOT NULL,
  `randomNumber` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;');

for ($i=1; $i <= 10000; $i++) {
    $db->insert('World', array(
        'id' => $i,
        'randomNumber' => $i
    ));
}
