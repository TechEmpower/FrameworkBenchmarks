<?php

/* - - - - - - - - - - - - - - - - - - - - -

 Title : PHP Quick Profiler Console Class
 Author : Created by Ryan Campbell
 URL : http://particletree.com

 Last Updated : April 26, 2009

 Description : This class serves as a wrapper around a global
 php variable, pqp_logs, that we have created.

- - - - - - - - - - - - - - - - - - - - - */

class Console {

	public static function init() {
		$GLOBALS['pqp_logs'] = array(
			'console' => array(),
			'logCount' => 0,
			'memoryCount' => 0,
			'errorCount' => 0,
			'speedCount' => 0);
	}

	/*-----------------------------------
	     LOG A VARIABLE TO CONSOLE
	------------------------------------*/

	public static function log($data) {
		$logItem = array(
			"data" => $data,
			"type" => 'log'
		);
		self::addToConsoleAndIncrement('logCount', $logItem);
	}

	/*---------------------------------------------------
	     LOG MEMORY USAGE OF VARIABLE OR ENTIRE SCRIPT
	-----------------------------------------------------*/

	public static function logMemory($object = false, $name = 'Memory Usage') {
		$memory = ($object and ! $object instanceOf \Controller) ? strlen(serialize($object)) : memory_get_usage();
		$logItem = array(
			"data" => $memory,
			"type" => 'memory',
			"name" => $name,
			"dataType" => gettype($object)
		);
		self::addToConsoleAndIncrement('memoryCount', $logItem);
	}

	/*-----------------------------------
	     LOG A PHP EXCEPTION OBJECT
	------------------------------------*/

	public static function logError($exception, $message) {
		$logItem = array(
			"data" => $message,
			"type" => 'error',
			"file" => $exception->getFile(),
			"line" => $exception->getLine()
		);
		self::addToConsoleAndIncrement('errorCount', $logItem);
	}

	/*------------------------------------
	     POINT IN TIME SPEED SNAPSHOT
	-------------------------------------*/

	public static function logSpeed($name = 'Point in Time') {
		$logItem = array(
			"data" => PhpQuickProfiler::getMicroTime(),
			"type" => 'speed',
			"name" => $name
		);
		self::addToConsoleAndIncrement('speedCount', $logItem);
	}

	/*-----------------------------------
	       RETURN  & MODIFY LOGS
	------------------------------------*/

	public static function addToConsoleAndIncrement($log, $item) {
		if(!isset($GLOBALS['pqp_logs'])) self::init();
		$GLOBALS['pqp_logs']['console'][] = $item;
		$GLOBALS['pqp_logs'][$log] += 1;
	}

	public static function getLogs() {
		if(!isset($GLOBALS['pqp_logs'])) self::init();
		return $GLOBALS['pqp_logs'];
	}

}

?>
