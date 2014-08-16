<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * Provides simple benchmarking and profiling. To display the statistics that
 * have been collected, load the `profiler/stats` [View]:
 *
 *     echo View::factory('profiler/stats');
 *
 * @package    Kohana
 * @category   Helpers
 * @author     Kohana Team
 * @copyright  (c) 2009-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_Profiler {

	/**
	 * @var  integer   maximium number of application stats to keep
	 */
	public static $rollover = 1000;

	/**
	 * @var  array  collected benchmarks
	 */
	protected static $_marks = array();

	/**
	 * Starts a new benchmark and returns a unique token. The returned token
	 * _must_ be used when stopping the benchmark.
	 *
	 *     $token = Profiler::start('test', 'profiler');
	 *
	 * @param   string  $group  group name
	 * @param   string  $name   benchmark name
	 * @return  string
	 */
	public static function start($group, $name)
	{
		static $counter = 0;

		// Create a unique token based on the counter
		$token = 'kp/'.base_convert($counter++, 10, 32);

		Profiler::$_marks[$token] = array
		(
			'group' => strtolower($group),
			'name'  => (string) $name,

			// Start the benchmark
			'start_time'   => microtime(TRUE),
			'start_memory' => memory_get_usage(),

			// Set the stop keys without values
			'stop_time'    => FALSE,
			'stop_memory'  => FALSE,
		);

		return $token;
	}

	/**
	 * Stops a benchmark.
	 *
	 *     Profiler::stop($token);
	 *
	 * @param   string  $token
	 * @return  void
	 */
	public static function stop($token)
	{
		// Stop the benchmark
		Profiler::$_marks[$token]['stop_time']   = microtime(TRUE);
		Profiler::$_marks[$token]['stop_memory'] = memory_get_usage();
	}

	/**
	 * Deletes a benchmark. If an error occurs during the benchmark, it is
	 * recommended to delete the benchmark to prevent statistics from being
	 * adversely affected.
	 *
	 *     Profiler::delete($token);
	 *
	 * @param   string  $token
	 * @return  void
	 */
	public static function delete($token)
	{
		// Remove the benchmark
		unset(Profiler::$_marks[$token]);
	}

	/**
	 * Returns all the benchmark tokens by group and name as an array.
	 *
	 *     $groups = Profiler::groups();
	 *
	 * @return  array
	 */
	public static function groups()
	{
		$groups = array();

		foreach (Profiler::$_marks as $token => $mark)
		{
			// Sort the tokens by the group and name
			$groups[$mark['group']][$mark['name']][] = $token;
		}

		return $groups;
	}

	/**
	 * Gets the min, max, average and total of a set of tokens as an array.
	 *
	 *     $stats = Profiler::stats($tokens);
	 *
	 * @param   array   $tokens profiler tokens
	 * @return  array   min, max, average, total
	 * @uses    Profiler::total
	 */
	public static function stats(array $tokens)
	{
		// Min and max are unknown by default
		$min = $max = array(
			'time' => NULL,
			'memory' => NULL);

		// Total values are always integers
		$total = array(
			'time' => 0,
			'memory' => 0);

		foreach ($tokens as $token)
		{
			// Get the total time and memory for this benchmark
			list($time, $memory) = Profiler::total($token);

			if ($max['time'] === NULL OR $time > $max['time'])
			{
				// Set the maximum time
				$max['time'] = $time;
			}

			if ($min['time'] === NULL OR $time < $min['time'])
			{
				// Set the minimum time
				$min['time'] = $time;
			}

			// Increase the total time
			$total['time'] += $time;

			if ($max['memory'] === NULL OR $memory > $max['memory'])
			{
				// Set the maximum memory
				$max['memory'] = $memory;
			}

			if ($min['memory'] === NULL OR $memory < $min['memory'])
			{
				// Set the minimum memory
				$min['memory'] = $memory;
			}

			// Increase the total memory
			$total['memory'] += $memory;
		}

		// Determine the number of tokens
		$count = count($tokens);

		// Determine the averages
		$average = array(
			'time' => $total['time'] / $count,
			'memory' => $total['memory'] / $count);

		return array(
			'min' => $min,
			'max' => $max,
			'total' => $total,
			'average' => $average);
	}

	/**
	 * Gets the min, max, average and total of profiler groups as an array.
	 *
	 *     $stats = Profiler::group_stats('test');
	 *
	 * @param   mixed   $groups single group name string, or array with group names; all groups by default
	 * @return  array   min, max, average, total
	 * @uses    Profiler::groups
	 * @uses    Profiler::stats
	 */
	public static function group_stats($groups = NULL)
	{
		// Which groups do we need to calculate stats for?
		$groups = ($groups === NULL)
			? Profiler::groups()
			: array_intersect_key(Profiler::groups(), array_flip( (array) $groups));

		// All statistics
		$stats = array();

		foreach ($groups as $group => $names)
		{
			foreach ($names as $name => $tokens)
			{
				// Store the stats for each subgroup.
				// We only need the values for "total".
				$_stats = Profiler::stats($tokens);
				$stats[$group][$name] = $_stats['total'];
			}
		}

		// Group stats
		$groups = array();

		foreach ($stats as $group => $names)
		{
			// Min and max are unknown by default
			$groups[$group]['min'] = $groups[$group]['max'] = array(
				'time' => NULL,
				'memory' => NULL);

			// Total values are always integers
			$groups[$group]['total'] = array(
				'time' => 0,
				'memory' => 0);

			foreach ($names as $total)
			{
				if ( ! isset($groups[$group]['min']['time']) OR $groups[$group]['min']['time'] > $total['time'])
				{
					// Set the minimum time
					$groups[$group]['min']['time'] = $total['time'];
				}
				if ( ! isset($groups[$group]['min']['memory']) OR $groups[$group]['min']['memory'] > $total['memory'])
				{
					// Set the minimum memory
					$groups[$group]['min']['memory'] = $total['memory'];
				}

				if ( ! isset($groups[$group]['max']['time']) OR $groups[$group]['max']['time'] < $total['time'])
				{
					// Set the maximum time
					$groups[$group]['max']['time'] = $total['time'];
				}
				if ( ! isset($groups[$group]['max']['memory']) OR $groups[$group]['max']['memory'] < $total['memory'])
				{
					// Set the maximum memory
					$groups[$group]['max']['memory'] = $total['memory'];
				}

				// Increase the total time and memory
				$groups[$group]['total']['time']   += $total['time'];
				$groups[$group]['total']['memory'] += $total['memory'];
			}

			// Determine the number of names (subgroups)
			$count = count($names);

			// Determine the averages
			$groups[$group]['average']['time']   = $groups[$group]['total']['time'] / $count;
			$groups[$group]['average']['memory'] = $groups[$group]['total']['memory'] / $count;
		}

		return $groups;
	}

	/**
	 * Gets the total execution time and memory usage of a benchmark as a list.
	 *
	 *     list($time, $memory) = Profiler::total($token);
	 *
	 * @param   string  $token
	 * @return  array   execution time, memory
	 */
	public static function total($token)
	{
		// Import the benchmark data
		$mark = Profiler::$_marks[$token];

		if ($mark['stop_time'] === FALSE)
		{
			// The benchmark has not been stopped yet
			$mark['stop_time']   = microtime(TRUE);
			$mark['stop_memory'] = memory_get_usage();
		}

		return array
		(
			// Total time in seconds
			$mark['stop_time'] - $mark['start_time'],

			// Amount of memory in bytes
			$mark['stop_memory'] - $mark['start_memory'],
		);
	}

	/**
	 * Gets the total application run time and memory usage. Caches the result
	 * so that it can be compared between requests.
	 *
	 *     list($time, $memory) = Profiler::application();
	 *
	 * @return  array  execution time, memory
	 * @uses    Kohana::cache
	 */
	public static function application()
	{
		// Load the stats from cache, which is valid for 1 day
		$stats = Kohana::cache('profiler_application_stats', NULL, 3600 * 24);

		if ( ! is_array($stats) OR $stats['count'] > Profiler::$rollover)
		{
			// Initialize the stats array
			$stats = array(
				'min'   => array(
					'time'   => NULL,
					'memory' => NULL),
				'max'   => array(
					'time'   => NULL,
					'memory' => NULL),
				'total' => array(
					'time'   => NULL,
					'memory' => NULL),
				'count' => 0);
		}

		// Get the application run time
		$time = microtime(TRUE) - KOHANA_START_TIME;

		// Get the total memory usage
		$memory = memory_get_usage() - KOHANA_START_MEMORY;

		// Calculate max time
		if ($stats['max']['time'] === NULL OR $time > $stats['max']['time'])
		{
			$stats['max']['time'] = $time;
		}

		// Calculate min time
		if ($stats['min']['time'] === NULL OR $time < $stats['min']['time'])
		{
			$stats['min']['time'] = $time;
		}

		// Add to total time
		$stats['total']['time'] += $time;

		// Calculate max memory
		if ($stats['max']['memory'] === NULL OR $memory > $stats['max']['memory'])
		{
			$stats['max']['memory'] = $memory;
		}

		// Calculate min memory
		if ($stats['min']['memory'] === NULL OR $memory < $stats['min']['memory'])
		{
			$stats['min']['memory'] = $memory;
		}

		// Add to total memory
		$stats['total']['memory'] += $memory;

		// Another mark has been added to the stats
		$stats['count']++;

		// Determine the averages
		$stats['average'] = array(
			'time'   => $stats['total']['time'] / $stats['count'],
			'memory' => $stats['total']['memory'] / $stats['count']);

		// Cache the new stats
		Kohana::cache('profiler_application_stats', $stats);

		// Set the current application execution time and memory
		// Do NOT cache these, they are specific to the current request only
		$stats['current']['time']   = $time;
		$stats['current']['memory'] = $memory;

		// Return the total application run time and memory usage
		return $stats;
	}

} // End Profiler
