<?php
/** @package    verysimple::Util */

/**
 * Static utility class for working with Dates
 *
 * @package		verysimple::Util
 * @author		Jason Hinkle
 * @copyright	1997-2011 VerySimple, Inc.
 * @license		LGPL http://www.gnu.org/licenses/lgpl.html
 * @version		1.0
 */
class VsDateUtil
{
	/** @var int one day in milliseconds */
	static $ONE_DAY = 86400000;
	
	/** @var int one hour in milliseconds */
	static $ONE_HOUR = 3600000;
	
	/** @var int one minute in milliseconds */
	static $ONE_MINUTE = 6000;
	
	/** @var int one second in milliseconds */
	static $ONE_SECOND = 1000;
	
	/**
	 * Return current date as string in the specified format
	 * @param string $format
	 */
	static function Today($format = "Y-m-d")
	{
		return self::Now($format);
	}
	
	/**
	 * Returns the timestamp for the beginning of DST for specified year
	 * @param int the 4-digit year (if not provide then the current year is used)
	 */
	static function DstStartNorthAmerica($year = null)
	{
		if (!$year) $year = date('Y');
		return strtotime('03/01/' . $year . ' second sunday');
	}
	
	/**
	 * Returns the timestamp for the end of DST for the specified year
	 * @param int the 4-digit year (if not provide then the current year is used)
	 */
	static function DstEndNorthAmerica($year = null)
	{
		if (!$year) $year = date('Y');
		return strtotime('11/01/' . $year . ' first sunday');
	}
	
	/**
	 * Return true if the date is within the DST observation range for North America
	 * @param int $timestamp (if not provided then the current server time will be used)
	 */
	static function IsDstNorthAmerica($timestamp = null)
	{
		if (!$timestamp) $timestamp = time();
		return $timestamp > self::DstStartNorthAmerica() && $timestamp < self::DstEndNorthAmerica();
	}
	
	/**
	 * Return current date/time as string in the specified format
	 * @param string $format
	 */
	static function Now($format = "Y-m-d H:i:s")
	{
		return date($format);
	}
	
	/**
	 * Return yesterday's date as string in the specified format
	 * @param string $format
	 */
	static function Yesterday($format = "Y-m-d")
	{
		return self::DaysAgo(1,$format);
	}

	/**
	 * Return tomorrow's date as string in the specified format
	 * @param string $format
	 */
	static function Tomorrow($format = "Y-m-d")
	{
		return self::DaysFromNow(1,$format);
	}
	
	/**
	 * Return the date/time 24 hours ago as string in the specified format
	 * @param string $format
	 */
	static function TwentyFourHoursAgo($format = "Y-m-d H:i:s")
	{
		return self::HoursAgo(24,$format);
	}

	/**
	 * Return the date/time 24 hours from now as string in the specified format
	 * @param string $format
	 */
	static function TwentyFourHoursFromNow($format = "Y-m-d H:i:s")
	{
		return self::HoursFromNow(24,$format);
	}
	
	/**
	 * Return date as a string the specified number of days ago
	 * @param int $days
	 * @param string $format
	 */
	static function DaysAgo($days, $format = "Y-m-d")
	{
		return date($format,strtotime(self::Now() . " - $days days"));
	}
	
	/**
	 * Return date as a string the specified number of days from now
	 * @param int $days
	 * @param string $format
	 */
	static function DaysFromNow($days, $format = "Y-m-d")
	{
		return date($format,strtotime(self::Now() . " + $days days"));
	}

	/**
	 * Return date/time as a string the specified number of hours ago
	 * @param int $hours
	 * @param string $format
	 */
	static function HoursAgo($hours, $format = "Y-m-d H:i:s")
	{
		return date($format,strtotime(self::Now() . " - $hours hours"));
	}
	
	/**
	 * Return date/time as a string the specified number of hours from now
	 * @param int $hours
	 * @param string $format
	 */
	static function HoursFromNow($hours, $format = "Y-m-d H:i:s")
	{
		return date($format,strtotime(self::Now() . " - $hours hours"));
	}
	
	
}

?>