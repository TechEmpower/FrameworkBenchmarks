<?php

namespace Fuel\Core;

import('phpquickprofiler/console', 'vendor');
import('phpquickprofiler/phpquickprofiler', 'vendor');

use \Console;
use \PhpQuickProfiler;

class Profiler
{

	protected static $profiler = null;

	protected static $query = null;

	public static function init()
	{
		if ( ! \Fuel::$is_cli and ! \Input::is_ajax() and ! static::$profiler)
		{
			static::$profiler = new PhpQuickProfiler(FUEL_START_TIME);
			static::$profiler->queries = array();
			static::$profiler->queryCount = 0;
			static::mark(__METHOD__.' Start');
			\Fuel::$profiling = true;
		}
	}

	public static function mark($label)
	{
		static::$profiler and Console::logSpeed($label);
	}

	public static function mark_memory($var = false, $name = 'PHP')
	{
		static::$profiler and Console::logMemory($var, $name);
	}

	public static function console($text)
	{
		static::$profiler and Console::log($text);
	}

	public static function output()
	{
		return static::$profiler ? static::$profiler->display(static::$profiler) : '';
	}

	public static function start($dbname, $sql)
	{
		if (static::$profiler)
		{
			static::$query = array(
				'sql' => \Security::htmlentities($sql),
				'time' => static::$profiler->getMicroTime(),
			);
			return true;
		}
	}

	public static function stop($text)
	{
		if (static::$profiler)
		{
			static::$query['time'] = (static::$profiler->getMicroTime() - static::$query['time']) *1000;
			array_push(static::$profiler->queries, static::$query);
			static::$profiler->queryCount++;
		}
	}

	public static function delete($text)
	{
		static::$query = null;
	}

	public static function app_total()
	{
		return array(
			microtime(true) - FUEL_START_TIME,
			memory_get_peak_usage() - FUEL_START_MEM
		);
	}
}
