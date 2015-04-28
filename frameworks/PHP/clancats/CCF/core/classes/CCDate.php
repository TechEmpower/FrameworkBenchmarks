<?php namespace Core;
/**
 * Date helper
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCDate 
{
	/**
	 * Get minutes in seconds
	 *
	 * @param int		$minutes
	 * @return int 
	 */
	public static function minutes( $minutes )
	{
		return $minutes * 60;
	}
	
	/**
	 * Get hours in seconds
	 *
	 * @param int		$hours
	 * @return int 
	 */
	public static function hours( $hours )
	{
		return $hours * 60 * 60;
	}
	
	/**
	 * Get days in seconds
	 *
	 * @param int		$days
	 * @return int 
	 */
	public static function days( $days )
	{
		return $days * 60 * 60 * 24;
	}
	
	/**
	 * Get weeks in seconds
	 *
	 * @param int		$weeks
	 * @return int 
	 */
	public static function weeks( $weeks )
	{
		return $weeks * 60 * 60 * 24 * 7;
	}
	
	/**
	 * Get months in seconds
	 *
	 * @param int		$months365
	 * @return int 
	 */
	public static function months( $months )
	{
		return $months * 60 * 60 * 24 * 30.4368;
	}
	
	/**
	 * Get years in seconds
	 *
	 * @param int		$years
	 * @return int 
	 */
	public static function years( $years )
	{
		return $years * 60 * 60 * 24 * 365;
	}
	
	/**
	 * format the date
	 */
	public static function format( $time, $format = 'd.m.Y  G:i' ) {
		return date( $format, $time );  
	}
	
	/**
	 * format a date without time
	 */
	public static function format_date( $date, $format = 'd.m.Y' ) {
		return date( $format, strtotime( $date ) ); 
	}
	
	/**
	 * to string relative string
	 *
	 * @param int	$ts
	 * @return string
	 */
	public static function relative($ts)
	{
		if(!ctype_digit($ts))
			$ts = strtotime($ts);
	
		$diff = time() - $ts;
		if($diff == 0)
			return __('Core::common.date.now');
		elseif($diff > 0)
		{
			$day_diff = floor($diff / 86400);
			if($day_diff == 0)
			{
				if($diff < 60) return __('Core::common.date.just_now');
				if($diff < 120) return __('Core::common.date.minute_ago');
				if($diff < 3600) return __('Core::common.date.minutes_ago', array( 'num' => floor($diff / 60) ) );
				if($diff < 7200) return __('Core::common.date.hour_ago');
				if($diff < 86400) return __('Core::common.date.hours_ago', array( 'num' => floor($diff / 3600) ) );
			}
			if($day_diff == 1) return __('Core::common.date.yesterday');
			if($day_diff < 7) return __('Core::common.date.days_ago', array( 'num' => $day_diff ) );
			if($day_diff < 31) return __('Core::common.date.weeks_ago', array( 'num' => ceil($day_diff / 7) ) );
			if($day_diff < 60) return __('Core::common.date.last_month');
			return date('F Y', $ts);
		}
		else
		{
			$diff = abs($diff);
			$day_diff = floor($diff / 86400);
			if($day_diff == 0)
			{
				if($diff < 120) return __('Core::common.date.in_minute');
				if($diff < 3600) return __('Core::common.date.in_minutes', array( 'num' => floor($diff / 60) ) );
				if($diff < 7200) return  __('Core::common.date.in_hour');
				if($diff < 86400) return __('Core::common.date.in_hours', array( 'num' => floor($diff / 3600) ) );
			}
			if($day_diff == 1) return __('Core::common.date.tomorrow');
			if($day_diff < 4) return date('l', $ts);
			if($day_diff < 7 + (7 - date('w'))) return 'next week';
			if(ceil($day_diff / 7) < 4) return 'in ' . ceil($day_diff / 7) . ' weeks';
			if(date('n', $ts) == date('n') + 1) return 'next month';
			return date('F Y', $ts);
		}
	}
}