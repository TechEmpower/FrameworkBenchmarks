<?php defined('SYSPATH') OR die('No direct script access.'); ?>

2012-06-25 14:45:57 --- EMERGENCY: Kohana_Exception [ 0 ]: Cannot create instances of abstract Controller_Template ~ SYSPATH/classes/Kohana/Request/Client/Internal.php [ 87 ] in /home/vagrant/web-app/kohana-3.3/system/classes/Kohana/Request/Client.php:114
2012-06-25 14:45:57 --- DEBUG: #0 /home/vagrant/web-app/kohana-3.3/system/classes/Kohana/Request/Client.php(114): Kohana_Request_Client_Internal->execute_request(Object(Mock_Request_92742025), Object(Response))
#1 /home/vagrant/web-app/kohana-3.3/system/tests/kohana/request/client/InternalTest.php(64): Kohana_Request_Client->execute(Object(Mock_Request_92742025))
#2 [internal function]: Kohana_Request_Client_InternalTest->test_response_failure_status('', 'Template', 'missing_action', 'kohana3/Templat...', 500)
#3 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/Framework/TestCase.php(738): ReflectionMethod->invokeArgs(Object(Kohana_Request_Client_InternalTest), Array)
#4 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/Framework/TestCase.php(628): PHPUnit_Framework_TestCase->runTest()
#5 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/Framework/TestResult.php(666): PHPUnit_Framework_TestCase->runBare()
#6 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/Framework/TestCase.php(576): PHPUnit_Framework_TestResult->run(Object(Kohana_Request_Client_InternalTest))
#7 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/Framework/TestSuite.php(757): PHPUnit_Framework_TestCase->run(Object(PHPUnit_Framework_TestResult))
#8 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/Framework/TestSuite.php(733): PHPUnit_Framework_TestSuite->runTest(Object(Kohana_Request_Client_InternalTest), Object(PHPUnit_Framework_TestResult))
#9 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/Framework/TestSuite.php(693): PHPUnit_Framework_TestSuite->run(Object(PHPUnit_Framework_TestResult), false, Array, Array, false)
#10 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/Framework/TestSuite.php(693): PHPUnit_Framework_TestSuite->run(Object(PHPUnit_Framework_TestResult), false, Array, Array, false)
#11 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/TextUI/TestRunner.php(305): PHPUnit_Framework_TestSuite->run(Object(PHPUnit_Framework_TestResult), false, Array, Array, false)
#12 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/TextUI/Command.php(188): PHPUnit_TextUI_TestRunner->doRun(Object(PHPUnit_Framework_TestSuite), Array)
#13 /home/vagrant/web-app/kohana-3.3/modules/unittest/vendor/phpunit/PHPUnit/TextUI/Command.php(129): PHPUnit_TextUI_Command->run(Array, true)
#14 /home/vagrant/web-app/kohana-3.3/modules/unittest/phpunit(68): PHPUnit_TextUI_Command::main()
#15 {main} in /home/vagrant/web-app/kohana-3.3/system/classes/Kohana/Request/Client.php:114