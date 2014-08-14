<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("IObserver.php");
require_once("verysimple/HTTP/RequestUtil.php");
/**
 * ObserverToBrowser is an implementation of IObserver that writes all
 * messages to a file
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2005 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class ObserveToFile implements IObserver
{
	private $filepath;
	private $eventtype;
	private $fh;
	private $fileIsOpen = false;
	
	public function __construct($filepath, $eventtype = null)
	{
		$this->filepath = $filepath;
		$this->eventtype = $eventtype;
		
		$this->Init();
	}
	
	public function __destruct()
	{
		@fclose($this->fh);
		$this->fileIsOpen = false;
	}
	
	function Init()
	{
		$this->fh = fopen($this->filepath,"a");
		$this->fileIsOpen = true;
		fwrite($this->fh,"DEBUG:\t". date("Y-m-d H:i:s:u") . "\t" . getmypid() . "\t########## ObserveToFile Initialized: " . RequestUtil::GetCurrentURL() . " ##########\r\n");
	}
	
	public function Observe($obj, $ltype = OBSERVE_INFO)
	{
		if (is_object($obj) || is_array($obj))
		{
			$msg = "<pre>" . print_r($obj, 1) . "</pre>";
		}
		else
		{
			$msg = $obj;
		}
		
		$msg = date("Y-m-d H:i:s:u") . "\t" . getmypid() . "\t" . str_replace(array("\t","\r","\n"),array(" "," "," "), $msg); 
		
		if ($this->eventtype == null || $this->eventtype & $ltype)
		{
			// this can occur if the file has been closed due to the php script terminating
			if (!$this->fileIsOpen) 
			{
				$this->Init();
				fwrite($this->fh, "WARN:\t". date("Y-m-d H:i:s:u") . "\tfilehandle was re-opened due to Observe being called after destruction\r\n");
			}
			
			switch ($ltype)
			{
				case OBSERVE_DEBUG:
					fwrite($this->fh, "DEBUG:\t$msg\r\n");
					break;
				case OBSERVE_QUERY:
					//fwrite($this->fh, "QUERY:\t" . $this->FormatTrace(debug_backtrace()) . " " . $msg . "\r\n");
					fwrite($this->fh, "QUERY:\t" . $msg . "\r\n");
					break;
				case OBSERVE_FATAL:
					fwrite($this->fh, "FATAL:\t$msg\r\n");
					break;
				case OBSERVE_INFO:
					fwrite($this->fh, "INFO:\t$msg\r\n");
					break;
				case OBSERVE_WARN:
					fwrite($this->fh, "WARN:\t$msg\r\n");
					break;
			}
			
		}
	}
	
	private function FormatTrace($tb, $join = " :: ", $show_lines = false)
	{
		$msg = "";
		$delim = "";
		
		$calling_function = "";
		$calling_line = "[?]";
		for ($x = count($tb); $x > 0; $x--)
		{
			$stack = $tb[$x-1];
			$s_file = isset($stack['file']) ? basename($stack['file']) : "[?]";
			$s_line = isset($stack['line']) ? $stack['line'] : "[?]";
			$s_function = isset($stack['function']) ? $stack['function'] : "";
			$s_class = isset($stack['class']) ? $stack['class'] : "";
			$s_type = isset($stack['type']) ? $stack['type'] : "";
			
			$msg .= $delim . "$calling_function" . ($show_lines ? " ($s_file Line $s_line)" : "");
			$calling_function = $s_class . $s_type . $s_function;
			
			$delim = $join;
		}
		
		return $msg;
		
	}
	
}

?>