<?php defined('SYSPATH') OR die('No direct script access.');
/**
* Log writer abstract class. All [Log] writers must extend this class.
*
* @package Kohana
* @category Logging
* @author Kohana Team
* @copyright (c) 2008-2012 Kohana Team
* @license http://kohanaframework.org/license
*/
abstract class Kohana_Log_Writer {

/**
* @var string timestamp format for log entries.
*
* Defaults to Date::$timestamp_format
*/
public static $timestamp;

/**
* @var string timezone for log entries
*
* Defaults to Date::$timezone, which defaults to date_default_timezone_get()
*/
public static $timezone;

/**
* Numeric log level to string lookup table.
* @var array
*/
protected $_log_levels = array(
LOG_EMERG => 'EMERGENCY',
LOG_ALERT => 'ALERT',
LOG_CRIT => 'CRITICAL',
LOG_ERR => 'ERROR',
LOG_WARNING => 'WARNING',
LOG_NOTICE => 'NOTICE',
LOG_INFO => 'INFO',
LOG_DEBUG => 'DEBUG',
);

/**
* @var int Level to use for stack traces
*/
public static $strace_level = LOG_DEBUG;

/**
* Write an array of messages.
*
* $writer->write($messages);
*
* @param array $messages
* @return void
*/
abstract public function write(array $messages);

/**
* Allows the writer to have a unique key when stored.
*
* echo $writer;
*
* @return string
*/
final public function __toString()
{
return spl_object_hash($this);
}

/**
* Formats a log entry.
*
* @param array $message
* @param string $format
* @return string
*/
public function format_message(array $message, $format = "time --- level: body in file:line")
{
$message['time'] = Date::formatted_time('@'.$message['time'], Log_Writer::$timestamp, Log_Writer::$timezone, TRUE);
$message['level'] = $this->_log_levels[$message['level']];

$string = strtr($format, array_filter($message, 'is_scalar'));

if (isset($message['additional']['exception']))
{
// Re-use as much as possible, just resetting the body to the trace
$message['body'] = $message['additional']['exception']->getTraceAsString();
$message['level'] = $this->_log_levels[Log_Writer::$strace_level];

$string .= PHP_EOL.strtr($format, array_filter($message, 'is_scalar'));
}

return $string;
}

}