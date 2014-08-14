<?php

foreach (glob('*Test.php') as $file)
{
	if ($file != 'AllValidationsTest.php')
		require $file;
}

class AllTests
{
	public static function suite()
	{
		$suite = new PHPUnit_Framework_TestSuite('PHPUnit');

		foreach (glob('*Test.php') as $file)
		{
			if ($file != 'AllValidationsTest.php')
				$suite->addTestSuite(substr($file,0,-4));
		}

		return $suite;
	}
}
?>
