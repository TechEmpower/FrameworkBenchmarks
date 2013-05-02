<?php defined('SYSPATH') or die('No direct script access.'); ?>

2012-03-26 09:13:53 --- ERROR: Kohana_Exception [ 0 ]: Attempted to load an invalid or missing module 'minion' at 'MODPATH/minion' ~ SYSPATH/classes/kohana/core.php [ 542 ]
2012-03-26 09:13:53 --- STRACE: Kohana_Exception [ 0 ]: Attempted to load an invalid or missing module 'minion' at 'MODPATH/minion' ~ SYSPATH/classes/kohana/core.php [ 542 ]
--
#0 /Volumes/Code/kohana/kohana-3.3/application/bootstrap.php(109): Kohana_Core::modules(Array)
#1 /Volumes/Code/kohana/kohana-3.3/index.php(102): require('/Volumes/Code/k...')
#2 {main}
2012-03-26 09:14:48 --- EMERGENCY: ErrorException [ 1 ]: Class 'CLI' not found ~ DOCROOT/index.php [ 109 ] in :
2012-03-26 09:14:48 --- ALERT: #0 [internal function]: Kohana_Core::shutdown_handler()
#1 {main} in :
2012-03-26 09:14:52 --- EMERGENCY: ErrorException [ 1 ]: Class 'CLI' not found ~ DOCROOT/index.php [ 109 ] in :
2012-03-26 09:14:52 --- ALERT: #0 [internal function]: Kohana_Core::shutdown_handler()
#1 {main} in :
2012-03-26 09:15:13 --- EMERGENCY: ErrorException [ 1 ]: Class 'CLI' not found ~ DOCROOT/index.php [ 109 ] in :
2012-03-26 09:15:13 --- ALERT: #0 [internal function]: Kohana_Core::shutdown_handler()
#1 {main} in :
2012-03-26 09:16:42 --- EMERGENCY: ErrorException [ 1 ]: Class 'Minion_Util' not found ~ MODPATH/minion/classes/Task/Help.php [ 20 ] in :
2012-03-26 09:16:42 --- ALERT: #0 [internal function]: Kohana_Core::shutdown_handler()
#1 {main} in :
2012-03-26 09:18:10 --- EMERGENCY: ErrorException [ 1 ]: Call to undefined method Minion_Task::compile_task_list() ~ MODPATH/minion/classes/Task/Help.php [ 20 ] in :
2012-03-26 09:18:10 --- ALERT: #0 [internal function]: Kohana_Core::shutdown_handler()
#1 {main} in :
2012-03-26 09:18:12 --- EMERGENCY: ErrorException [ 1 ]: Call to undefined method Minion_Task::compile_task_list() ~ MODPATH/minion/classes/Task/Help.php [ 20 ] in :
2012-03-26 09:18:12 --- ALERT: #0 [internal function]: Kohana_Core::shutdown_handler()
#1 {main} in :