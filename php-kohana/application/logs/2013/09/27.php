<?php defined('SYSPATH') OR die('No direct script access.'); ?>

2013-09-27 12:59:36 --- EMERGENCY: Database_Exception [ 2 ]: mysql_connect(): No such file or directory ~ MODPATH/database/classes/Kohana/Database/MySQL.php [ 67 ] in /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php:171
2013-09-27 12:59:36 --- DEBUG: #0 /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php(171): Kohana_Database_MySQL->connect()
#1 /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/Query.php(251): Kohana_Database_MySQL->query(1, 'SELECT * FROM W...', false, Array)
#2 /home/sbandy/FrameworkBenchmarks/php-kohana/application/classes/Controller/Bench.php(24): Kohana_Database_Query->execute()
#3 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Controller.php(84): Controller_Bench->action_db()
#4 [internal function]: Kohana_Controller->execute()
#5 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request/Client/Internal.php(97): ReflectionMethod->invoke(Object(Controller_Bench))
#6 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request/Client.php(114): Kohana_Request_Client_Internal->execute_request(Object(Request), Object(Response))
#7 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request.php(990): Kohana_Request_Client->execute(Object(Request))
#8 /home/sbandy/FrameworkBenchmarks/php-kohana/index.php(118): Kohana_Request->execute()
#9 {main} in /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php:171
2013-09-27 12:59:36 --- EMERGENCY: Database_Exception [ 2 ]: mysql_connect(): No such file or directory ~ MODPATH/database/classes/Kohana/Database/MySQL.php [ 67 ] in /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php:171
2013-09-27 12:59:36 --- DEBUG: #0 /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php(171): Kohana_Database_MySQL->connect()
#1 /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/Query.php(251): Kohana_Database_MySQL->query(1, 'SELECT * FROM W...', false, Array)
#2 /home/sbandy/FrameworkBenchmarks/php-kohana/application/classes/Controller/Bench.php(24): Kohana_Database_Query->execute()
#3 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Controller.php(84): Controller_Bench->action_db()
#4 [internal function]: Kohana_Controller->execute()
#5 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request/Client/Internal.php(97): ReflectionMethod->invoke(Object(Controller_Bench))
#6 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request/Client.php(114): Kohana_Request_Client_Internal->execute_request(Object(Request), Object(Response))
#7 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request.php(990): Kohana_Request_Client->execute(Object(Request))
#8 /home/sbandy/FrameworkBenchmarks/php-kohana/index.php(118): Kohana_Request->execute()
#9 {main} in /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php:171
2013-09-27 12:59:36 --- EMERGENCY: Database_Exception [ 2 ]: mysql_connect(): No such file or directory ~ MODPATH/database/classes/Kohana/Database/MySQL.php [ 67 ] in /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php:171
2013-09-27 12:59:36 --- DEBUG: #0 /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php(171): Kohana_Database_MySQL->connect()
#1 /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/Query.php(251): Kohana_Database_MySQL->query(1, 'SELECT * FROM `...', false, Array)
#2 /home/sbandy/FrameworkBenchmarks/php-kohana/application/classes/Controller/Bench.php(35): Kohana_Database_Query->execute()
#3 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Controller.php(84): Controller_Bench->action_fortunes()
#4 [internal function]: Kohana_Controller->execute()
#5 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request/Client/Internal.php(97): ReflectionMethod->invoke(Object(Controller_Bench))
#6 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request/Client.php(114): Kohana_Request_Client_Internal->execute_request(Object(Request), Object(Response))
#7 /home/sbandy/FrameworkBenchmarks/php-kohana/system/classes/Kohana/Request.php(990): Kohana_Request_Client->execute(Object(Request))
#8 /home/sbandy/FrameworkBenchmarks/php-kohana/index.php(118): Kohana_Request->execute()
#9 {main} in /home/sbandy/FrameworkBenchmarks/php-kohana/modules/database/classes/Kohana/Database/MySQL.php:171